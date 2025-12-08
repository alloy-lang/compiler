mod behavior;
mod diagnostics;
mod expr;
mod fql;
mod pattern;
mod r#trait;
mod type_definition;
mod type_reference;
mod type_variable;

use alloy_hir as hir;
use non_empty_vec::NonEmpty;

pub use behavior::*;
pub use diagnostics::*;
pub use expr::*;
pub use fql::*;
pub use pattern::*;
pub use r#trait::*;
pub use type_definition::*;
pub use type_reference::*;
pub use type_variable::*;

/// Helper function to resolve a cross-module expression reference
fn resolve_cross_module_expression(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: Fql<hir::Expression>,
) -> Result<Fql<hir::Expression>, TypeResolutionError> {
    let module_slug = fqn.module_slug();
    if fqn.sub_path.is_empty() {
        let Some(other_module_id) = db.find_module_by_slug(&*module_slug) else {
            return Err(TypeResolutionError::UnknownModule {
                module_slug: module_slug.to_string(),
                source_ref: source_ref.into(),
            });
        };
        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let Some((expr_id, _)) =
            hir_module.get_expression_by_name(&fqn.name, alloy_scope::Scopes::ROOT)
        else {
            return Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id: other_module_id,
                path: fqn.segments(),
            });
        };
        return Ok(Fql::new(other_module_id, expr_id));
    }

    // Build the full path: module + name + sub_path (except last element)
    let full_path: NonEmpty<_> = fqn.segments();

    // Try different splits: start from the end and work backwards
    // For "std::option::Option::Some", try:
    //   1. module="std::option::Option" (probably doesn't exist)
    //   2. module="std::option", type="Option" (this should work!)
    //   3. module="std", type="option" (probably not a type)
    let full_path_length = full_path.len().into();
    for split_point in (1..=full_path_length).rev() {
        let module_path = &full_path[..split_point];
        let type_name = if split_point < full_path_length {
            &full_path[split_point]
        } else {
            continue; // No type name after this split
        };

        // Try to find this module
        let module_slug = module_path
            .iter()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join("::");

        let Some(other_module_id) = db.find_module_by_slug(&module_slug) else {
            continue; // Try next split
        };

        let (hir_module, _) = hir::lower_file(db, other_module_id);

        // Try to find the type in this module
        let Some((expr_id, _)) =
            hir_module.get_expression_by_name(type_name, alloy_scope::Scopes::ROOT)
        else {
            // we found the module, but not the expression
            continue;
        };

        return Ok(Fql::new(other_module_id, expr_id));
    }

    Err(TypeResolutionError::UnknownModule {
        module_slug: module_slug.to_string(),
        source_ref: source_ref.into(),
    })
}

/// Helper function to resolve a cross-module type definition reference
///
/// This handles qualified variant references by trying different ways to split
/// the path into (module, type, variant).
///
/// For example, `std::option::Option::Some` could be split as:
/// - module: "std::option", type: "Option", variant: "Some"
/// - module: "std", type: "option", variant: "Option" (invalid - Option is not a variant)
pub(crate) fn resolve_cross_module_type_definition(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: EPTFql,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    let mut module_slug = fqn.module_slug();
    if fqn.sub_path.is_empty() {
        let Some(other_module_id) = db.find_module_by_slug(&*module_slug) else {
            return Err(TypeResolutionError::UnknownModule {
                module_slug: module_slug.to_string(),
                source_ref,
            });
        };
        let (hir_module, _) = hir::lower_file(db, other_module_id);

        let Some((type_def_id, _)) =
            hir_module.get_type_definition_by_name(&fqn.name, alloy_scope::Scopes::ROOT)
        else {
            return match source_ref {
                EPTFql::Expression(fql) => Err(TypeResolutionError::UnknownExpressionReference {
                    source_ref: fql,
                    module_id: other_module_id,
                    path: fqn.segments(),
                }),
                EPTFql::Pattern(fql) => Err(TypeResolutionError::UnknownPatternReference {
                    source_ref: fql,
                    module_id: other_module_id,
                    path: fqn.segments(),
                }),
                EPTFql::TypeReference(fql) => Err(TypeResolutionError::UnknownTypeReference {
                    source_ref: fql,
                    module_id: other_module_id,
                    path: fqn.segments(),
                }),
            };
        };
        return Ok(Fql::new(other_module_id, type_def_id));
    }

    // Build the full path: module + name + sub_path (except last element)
    let full_path: NonEmpty<_> = fqn.segments();
    println!("full_path: {:?}", full_path);

    // The last element of full_path is always the variant name
    let variant_name = full_path.last();

    // Try different splits: start from the end and work backwards
    // For "std::option::Option::Some", try:
    //   1. module="std::option::Option" (probably doesn't exist)
    //   2. module="std::option", type="Option" (this should work!)
    //   3. module="std", type="option" (probably not a type)
    let full_path_length = full_path.len().into();
    for split_point in (1..=full_path_length).rev() {
        let module_path = &full_path[..split_point];
        let type_name = if split_point < full_path_length {
            &full_path[split_point]
        } else {
            continue; // No type name after this split
        };

        // Try to find this module
        module_slug = module_path
            .iter()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join("::");

        println!("module_slug: {:?}", module_slug);
        let Some(other_module_id) = db.find_module_by_slug(&module_slug) else {
            continue; // Try next split
        };

        let (hir_module, _) = hir::lower_file(db, other_module_id);

        // Try to find the type in this module
        let Some((type_def_id, type_def)) =
            hir_module.get_type_definition_by_name(type_name, alloy_scope::Scopes::ROOT)
        else {
            // we found the module, but not the type definition
            continue;
        };

        // Check if this type has the requested variant
        if type_def.kind.has_variant(variant_name) {
            return Ok(Fql::new(other_module_id, type_def_id));
        }
    }

    println!("error module_slug: {:?}", module_slug);
    Err(TypeResolutionError::UnknownModule {
        module_slug: module_slug.to_string(),
        source_ref,
    })
}
