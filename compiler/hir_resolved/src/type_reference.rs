use crate::{cross_module_resolver, EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;

pub fn resolve_type_reference_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::TypeReference>> {
    match path {
        hir::Path::ThisModule {
            name,
            scope: target_scope,
            ..
        } => get_type_reference_by_name(db, current_module_id, name, *target_scope),
        hir::Path::OtherModule(fqn) => resolve_cross_module_type_reference(db, fqn),
        hir::Path::Unknown(_) => None,
    }
}

fn get_type_reference_by_name(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::TypeReference>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let (type_idx, _) = hir_module.get_type_reference_by_name(name, scope)?;
    Some(Fql::new(module_id, type_idx))
}

// ============================================================================
// Type Reference Lookup
// ============================================================================

struct TypeReferenceLookup;

impl cross_module_resolver::ModuleLookup<hir::TypeReference> for TypeReferenceLookup {
    type Item = hir::TypeReference;

    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<hir::TypeReference>, Self::Item)> {
        hir_module
            .get_type_reference_by_name(name, Scopes::ROOT)
            .map(|(id, type_ref)| (id, type_ref.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> TypeResolutionError {
        let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
            panic!("Trait resolution requires TypeReference");
        };
        TypeResolutionError::UnknownTypeReference {
            source_ref: source_ref.into(),
            module_id,
            path,
        }
    }
}

/// Resolve a cross-module type reference
fn resolve_cross_module_type_reference(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::TypeReference>> {
    cross_module_resolver::resolve_cross_module_optional::<hir::TypeReference, TypeReferenceLookup>(
        db, fqn,
    )
}
