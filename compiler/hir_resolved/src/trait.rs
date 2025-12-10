use crate::{cross_module_resolver, EPTFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::{ne_vec, NonEmpty};

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

// ============================================================================
// Trait Lookup
// ============================================================================

struct TraitLookup;

impl cross_module_resolver::ModuleLookup<hir::Trait> for TraitLookup {
    type Item = hir::Trait;

    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<hir::Trait>, Self::Item)> {
        hir_module
            .get_trait_by_name(name)
            .map(|(id, trait_)| (id, trait_.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTFql>,
        _module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> TypeResolutionError {
        let EPTFql::TypeReference(source_ref) = source_ref.into() else {
            panic!("Trait resolution requires TypeReference");
        };

        // Build the Fqn from the path
        let path_len: usize = path.len().into();
        let fqn = if path_len == 1 {
            // Single element path: just the trait name
            // For Fqn, module field is NonEmpty<Name>, so we need to convert
            hir::Fqn::new(
                ne_vec![path[0].clone()],
                path[0].clone(),
                Vec::<hir::Name>::new(),
            )
        } else {
            // Multi-element path: construct module as NonEmpty, last is trait name, middle is sub_path
            hir::Fqn::new(
                ne_vec![path[0].clone()],
                path.last().clone(),
                path[1..path_len - 1].to_vec(),
            )
        };

        TypeResolutionError::UnknownTraitName { source_ref, fqn }
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
    cross_module_resolver::resolve_cross_module::<hir::Trait, TraitLookup>(db, fqn, source_ref)
}
