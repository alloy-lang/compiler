use crate::{resolver, EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::{Idx, RawIdx};
use non_empty_vec::{ne_vec, NonEmpty};

pub fn resolve_type_reference_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::TypeReference>> {
    resolver::resolve_by_path::<hir::TypeReference, TypeReferenceResolver>(
        db,
        current_module_id,
        path,
        Fql {
            module_id: current_module_id,
            local_id: Idx::<hir::TypeReference>::from_raw(RawIdx::from_u32(u32::MAX)),
        },
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
    ) -> TypeResolutionError {
        let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
            panic!("TypeReference resolution requires TypeReference");
        };

        TypeResolutionError::UnknownTypeReference {
            source_ref,
            module_id,
            path,
        }
    }

    fn validate(
        _db: &dyn hir::HirDatabase,
        source_ref: impl Into<EPTrFql>,
        item_fql: Fql<hir::TypeReference>,
        subname: Option<hir::Name>,
    ) -> Option<TypeResolutionError> {
        if let Some(subname) = subname {
            let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
                panic!("TypeReference resolution requires TypeReference");
            };

            return Some(TypeResolutionError::UnknownTypeReference {
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
    use alloy_hir::{ResolutionIdx, ResolutionKind};
    use alloy_scope::Scopes;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::{Idx, RawIdx};

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

        let actual_ref = resolve_type_reference_by_path(&db, module_id, &path)
            .expect("must find type reference");
        let expected = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual_ref);
    }

    #[test]
    fn test_resolve_type_reference_cross_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
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
            module: ne_vec![hir::Name::new("types")],
            name: hir::Name::new("MyType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn, vec![]);

        let actual = resolve_type_reference_by_path(&db, module_id, &path);
        assert!(
            actual.is_none(),
            "Should return None for unknown cross-module type"
        );
    }

    #[test]
    fn test_resolve_type_reference_cross_module_returns_none_for_unknown() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
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
            module: ne_vec![hir::Name::new("types")],
            name: hir::Name::new("UnknownType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn, vec![]);

        let actual = resolve_type_reference_by_path(&db, module_id, &path);
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

        let unknown_path = hir::Path::Unknown(ne_vec![
            hir::Name::new("unknown"),
            hir::Name::new("path"),
            hir::Name::new("Type"),
        ]);

        let result = resolve_type_reference_by_path(&db, module_id, &unknown_path);
        assert!(result.is_none(), "Unknown path should return None");
    }
}
