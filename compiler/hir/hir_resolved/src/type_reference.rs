use crate::{resolver, EPTrFql, Fql, HirResolutionError};
use alloy_hir_def as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::{ne_vec, NonEmpty};

pub fn resolve_type_reference_by_path(
    db: &dyn hir::HirDefDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    source_ref: impl Into<EPTrFql>,
) -> Option<Fql<hir::TypeReference>> {
    resolver::resolve_by_path::<hir::TypeReference, TypeReferenceResolver>(
        db,
        current_module_id,
        path,
        source_ref,
    )
    .ok()
}

// ============================================================================
// TypeReference Resolver
// ============================================================================

struct TypeReferenceResolver;

impl resolver::Resolver<hir::TypeReference> for TypeReferenceResolver {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        scope: ScopeIdx,
    ) -> Option<(Idx<hir::TypeReference>, hir::TypeReference)> {
        hir_module
            .get_type_reference_by_name(name, scope)
            .map(|(id, expr)| (id, expr.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> HirResolutionError {
        let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
            panic!("TypeReference resolution requires TypeReference");
        };

        HirResolutionError::UnknownTypeReference {
            source_ref,
            module_id,
            path,
        }
    }

    fn validate(
        _db: &dyn hir::HirDefDatabase,
        source_ref: impl Into<EPTrFql>,
        item_fql: Fql<hir::TypeReference>,
        subname: Option<hir::Name>,
    ) -> Option<HirResolutionError> {
        if let Some(subname) = subname {
            let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
                panic!("TypeReference resolution requires TypeReference");
            };

            return Some(HirResolutionError::UnknownTypeReference {
                source_ref,
                module_id: item_fql.module_id,
                path: ne_vec![subname],
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use alloy_hir_def::{ResolutionIdx, ResolutionKind};
    use alloy_scope::Scopes;
    use alloy_test_harness::idx;
    use alloy_workspace::WorkspaceDatabase;

    #[test]
    fn test_resolve_type_reference_this_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typedef MyType = Thing
    typeof dummy : MyType
            ",
        );

        let path = hir::Path::ThisModule {
            name: "dummy".into(),
            subname: None,
            scope: Scopes::ROOT,
            resolution_kind: ResolutionKind::TypeDefinition,
            resolution_idx: ResolutionIdx::Unresolved,
        };

        let source_ref: Fql<hir::TypeReference> = Fql::new(module_id, idx!(0));
        let actual_ref = resolve_type_reference_by_path(&db, module_id, &path, source_ref)
            .expect("must find type reference");
        let expected = Fql::new(module_id, idx!(0));

        assert_eq!(expected, actual_ref);
    }

    #[test]
    fn test_resolve_type_reference_cross_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let types_module_id = db.add_module(
            "types",
            camino::Utf8Path::new("./types.alloy"),
            r"
    typedef MyType = Thing
            ",
        );

        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import types
    typeof dummy : types::MyType
            ",
        );

        // Test that unknown cross-module type references return None
        let fqn = hir::Fqn {
            module_id: types_module_id,
            module: ne_vec![hir::Name::new("types")],
            name: hir::Name::new("MyType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn, vec![]);

        let source_ref: Fql<hir::TypeReference> = Fql::new(module_id, idx!(0));
        let actual = resolve_type_reference_by_path(&db, module_id, &path, source_ref);
        assert!(
            actual.is_none(),
            "Should return None for unknown cross-module type"
        );
    }

    #[test]
    fn test_resolve_type_reference_cross_module_returns_none_for_unknown() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let types_module_id = db.add_module(
            "types",
            camino::Utf8Path::new("./types.alloy"),
            r"
    typedef MyType = Thing
            ",
        );

        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import types
    typeof dummy : types::UnknownType
            ",
        );

        // Test that unknown cross-module type references return None
        let fqn = hir::Fqn {
            module_id: types_module_id,
            module: ne_vec![hir::Name::new("types")],
            name: hir::Name::new("UnknownType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn, vec![]);

        let source_ref: Fql<hir::TypeReference> = Fql::new(module_id, idx!(0));
        let actual = resolve_type_reference_by_path(&db, module_id, &path, source_ref);
        assert!(
            actual.is_none(),
            "Should return None for unknown cross-module type"
        );
    }

    #[test]
    fn test_resolve_type_reference_unknown_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typeof foo : unknown::path::Type
            ",
        );

        let unknown_path = hir::Path::UnknownReference(ne_vec![
            hir::Name::new("unknown"),
            hir::Name::new("path"),
            hir::Name::new("Type"),
        ]);

        let source_ref: Fql<hir::TypeReference> = Fql::new(module_id, idx!(0));
        let result = resolve_type_reference_by_path(&db, module_id, &unknown_path, source_ref);
        assert!(result.is_none(), "Unknown path should return None");
    }
}
