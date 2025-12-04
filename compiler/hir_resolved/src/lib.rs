mod expr;
mod fql;
mod pattern;

use alloy_hir as hir;
pub use expr::*;
pub use fql::*;
use non_empty_vec::NonEmpty;
pub use pattern::*;

/// Helper function to resolve a cross-module expression reference
fn resolve_cross_module_expression(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::Expression>> {
    if fqn.sub_path.is_empty() {
        let module_slug = fqn.module_slug();
        let other_module_id = db.find_module_by_slug(&*module_slug)?;
        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let (expr_id, _) =
            hir_module.get_expression_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
        return Some(Fql::new(other_module_id, expr_id));
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
            continue; // Try next split
        };

        return Some(Fql::new(other_module_id, expr_id));
    }

    None
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
) -> Option<Fql<hir::TypeDefinition>> {
    // If sub_path is empty, just look up the type directly
    if fqn.sub_path.is_empty() {
        let module_slug = fqn.module_slug();
        let other_module_id = db.find_module_by_slug(&*module_slug)?;
        let (hir_module, _) = hir::lower_file(db, other_module_id);

        let (type_def_id, _) =
            hir_module.get_type_definition_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
        return Some(Fql::new(other_module_id, type_def_id));
    }

    // Build the full path: module + name + sub_path (except last element)
    let full_path: NonEmpty<_> = fqn.segments();

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
        let Some((type_def_id, type_def)) =
            hir_module.get_type_definition_by_name(type_name, alloy_scope::Scopes::ROOT)
        else {
            continue; // Try next split
        };

        // Check if this type has the requested variant
        if type_def.kind.has_variant(variant_name) {
            return Some(Fql::new(other_module_id, type_def_id));
        }
    }

    None
}
