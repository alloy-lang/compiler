use crate::{cross_module_resolver, EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use cross_module_resolver::resolve_cross_module_optional;
use non_empty_vec::NonEmpty;

pub fn resolve_type_reference_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::TypeReference>> {
    match path {
        hir::Path::ThisModule { name, scope, .. } => {
            get_type_reference_by_name(db, current_module_id, name, *scope)
        }
        hir::Path::OtherModule(fqn) => {
            resolve_cross_module_optional::<hir::TypeReference, TypeReferenceLookup>(db, fqn)
        }
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
    ) -> Option<(hir::TypeIdx, Self::Item)> {
        hir_module
            .get_type_reference_by_name(name, Scopes::ROOT)
            .map(|(id, type_ref)| (id, type_ref.clone()))
    }

    fn validate(_item: Self::Item, _remaining_path: &[hir::Name]) -> bool {
        true // Default: no validation needed
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
            source_ref,
            module_id,
            path,
        }
    }

    fn validation_error(
        _source_ref: impl Into<EPTrFql>,
        _module_id: ModuleId,
        _path: NonEmpty<hir::Name>,
        _remaining_path: &[hir::Name],
        _item_id: hir::TypeIdx,
    ) -> TypeResolutionError {
        unreachable!("type references don't have sub items")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
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
            module: non_empty_vec::ne_vec![hir::Name::new("types")],
            name: hir::Name::new("MyType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn);

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
            module: non_empty_vec::ne_vec![hir::Name::new("types")],
            name: hir::Name::new("UnknownType"),
            sub_path: None,
        };
        let path = hir::Path::OtherModule(fqn);

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

        let unknown_path = hir::Path::Unknown(non_empty_vec::ne_vec![
            hir::Name::new("unknown"),
            hir::Name::new("path"),
            hir::Name::new("Type"),
        ]);

        let result = resolve_type_reference_by_path(&db, module_id, &unknown_path);
        assert!(result.is_none(), "Unknown path should return None");
    }
}
