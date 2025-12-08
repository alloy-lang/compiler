use crate::{Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::ne_vec;

pub struct Trait {}

pub fn resolve_trait_by_ref_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> Result<Fql<hir::Trait>, TypeResolutionError> {
    let source_ref = Fql::new(module_id, type_idx);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Named(path) => resolve_trait_by_path(db, module_id, path, source_ref),
        hir::TypeReference::Bounded { base, .. } => {
            Err(TypeResolutionError::BoundedTraitReference {
                source_ref,
                target_ref: Fql::new(module_id, *base),
            })
        }
        _ => unreachable!(
            "Invalid type reference for trait resolution: {:?}",
            type_ref
        ),
    }
}

/// Resolve a trait from a hir::Path
///
/// This handles both ThisModule and OtherModule paths, resolving them to Fql<Trait>
pub fn resolve_trait_by_path(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
    source_ref: Fql<hir::TypeReference>,
) -> Result<Fql<hir::Trait>, TypeResolutionError> {
    match path {
        hir::Path::ThisModule { name, subname, .. } => {
            if subname.is_some() {
                unreachable!("Traits can't be qualified");
            }
            let (hir_module, _) = hir::lower_file(db, module_id);
            let Some((trait_idx, _)) = hir_module.get_trait_by_name(name) else {
                return Err(TypeResolutionError::UnknownTraitReference {
                    source_ref,
                    module_id,
                    path: ne_vec![name.clone()],
                });
            };
            Ok(Fql::new(module_id, trait_idx))
        }
        hir::Path::OtherModule(fqn) => resolve_cross_module_trait(db, fqn, source_ref),
        hir::Path::Unknown(names) => Err(TypeResolutionError::UnknownTraitReference {
            source_ref,
            module_id,
            path: names.clone(),
        }),
    }
}

/// Helper function to resolve a cross-module trait reference
///
/// This handles qualified trait references by trying different ways to split
/// the path into (module, trait).
///
/// For example, `std::option::Trait1` could be split as:
/// - module: "std::option", trait: "Trait1"
/// - module: "std", trait: "option" (invalid - option is not a trait)
fn resolve_cross_module_trait(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: Fql<hir::TypeReference>,
) -> Result<Fql<hir::Trait>, TypeResolutionError> {
    let module_slug = fqn.module_slug();
    // If sub_path is empty, just look up the trait directly
    if fqn.sub_path.is_empty() {
        let Some(other_module_id) = db.find_module_by_slug(&*module_slug) else {
            return Err(TypeResolutionError::UnknownTraitModule {
                module_slug: module_slug.to_string(),
                source_ref,
            });
        };
        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let Some((trait_idx, _)) = hir_module.get_trait_by_name(&fqn.name) else {
            return Err(TypeResolutionError::UnknownTraitName {
                source_ref,
                fqn: fqn.clone(),
            });
        };
        return Ok(Fql::new(other_module_id, trait_idx));
    }

    // Build the full path: module + name + sub_path
    let full_path: non_empty_vec::NonEmpty<_> = fqn.segments();

    // Try different splits: start from the end and work backwards
    // The last element is always the trait name
    let full_path_length = full_path.len().into();
    for split_point in (1..full_path_length).rev() {
        let module_path = &full_path[..split_point];
        let trait_name = &full_path[split_point];

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

        // Try to find the trait in this module
        let Some((trait_idx, _)) = hir_module.get_trait_by_name(trait_name) else {
            // we found the module, but not the trait
            continue; // Try next split
        };

        return Ok(Fql::new(other_module_id, trait_idx));
    }

    Err(TypeResolutionError::UnknownTraitModule {
        module_slug: module_slug.to_string(),
        source_ref,
    })
}
