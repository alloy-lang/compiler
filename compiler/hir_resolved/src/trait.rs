use crate::{cross_module_resolver, EPTrFql, Expression, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use cross_module_resolver::resolve_cross_module_optional;
use la_arena::Idx;
use non_empty_vec::{ne_vec, NonEmpty};

pub struct Trait {}

#[salsa::tracked]
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
fn resolve_trait_by_path(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
    source_ref: Fql<hir::TypeReference>,
) -> Result<Fql<hir::Trait>, TypeResolutionError> {
    let error_path = match path {
        hir::Path::ThisModule { name, subname, .. } => {
            if subname.is_some() {
                unreachable!("Traits can't be qualified");
            }
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((trait_idx, _)) = hir_module.get_trait_by_name(name) {
                return Ok(Fql::new(module_id, trait_idx));
            };

            // lowering error
            ne_vec![name.clone()]
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(fql) = resolve_cross_module_optional::<hir::Trait, TraitLookup>(db, fqn) {
                return Ok(fql);
            };

            if db.find_module_by_slug(&fqn.module_slug()).is_none() {
                return Err(TypeResolutionError::UnknownModule {
                    module_slug: fqn.module_slug(),
                    source_ref: source_ref.into(),
                });
            }

            fqn.segments()
        }
        hir::Path::Unknown(names) => names.clone(),
    };

    Err(TypeResolutionError::UnknownTraitReference {
        source_ref,
        module_id,
        path: error_path,
    })
}

pub(crate) fn resolve_abstract_trait_member_by_path(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
) -> Option<Expression> {
    if let hir::Path::ThisModule { name, scope, .. } = path {
        let (hir_module, _) = hir::lower_file(db, module_id);

        // Check if this is an abstract trait member reference
        // (within a trait scope, referencing a member with a type annotation but no implementation)
        if let Some((trait_idx, trait_def)) = hir_module.find_trait_containing_scope(*scope) {
            // Check if this name is an abstract trait member
            for (member_name, type_annotation_idx) in trait_def.abstract_members() {
                if member_name == name {
                    return Some(Expression::AbstractTraitMemberRef {
                        trait_fql: Fql::new(module_id, trait_idx),
                        member_name: member_name.clone(),
                        type_annotation: Fql::new(module_id, type_annotation_idx),
                    });
                }
            }
        }
    }
    None
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

        TypeResolutionError::UnknownTraitReference {
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
        _item_id: hir::TraitIdx,
    ) -> TypeResolutionError {
        unreachable!("traits don't have sub items")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::RawIdx;

    fn find_trait(db: &dyn hir::HirDatabase, module_id: ModuleId) -> Fql<hir::Trait> {
        resolve_trait_by_ref_id(db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("must find trait")
    }

    fn find_trait_error(db: &dyn hir::HirDatabase, module_id: ModuleId) -> TypeResolutionError {
        resolve_trait_by_ref_id(db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect_err("must fail to find trait")
    }

    #[test]
    fn test_resolve_trait_this_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    trait MyTrait where
    end
    
    typeof dummy : MyTrait
            ",
        );

        let actual_trait = find_trait(&db, module_id);
        let expected = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual_trait);
    }

    #[test]
    fn test_resolve_trait_unknown() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
        typeof dummy : UnknownTrait
        ",
        );

        let actual_err = find_trait_error(&db, module_id);

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let expected = TypeResolutionError::UnknownTraitReference {
            source_ref,
            module_id,
            path: ne_vec!["UnknownTrait".into()],
        };

        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_trait_cross_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let other_module_id = db.add_module(
            "traits",
            camino::Utf8Path::new("./traits.alloy"),
            r"
    trait MyTrait where
    end
            ",
        );

        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import traits::MyTrait
    typeof dummy : MyTrait
            ",
        );

        let actual_trait = find_trait(&db, module_id);
        let expected = Fql::new(other_module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual_trait);
    }

    #[test]
    fn test_resolve_trait_cross_module_extra_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let other_module_id = db.add_module(
            "traits",
            camino::Utf8Path::new("./traits.alloy"),
            r"
    trait MyTrait where
    end
            ",
        );

        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import traits::MyTrait
    typeof dummy : MyTrait::extra_junk
            ",
        );

        let actual_trait = find_trait(&db, module_id);
        let expected = Fql::new(other_module_id, Idx::from_raw(RawIdx::from_u32(1)));

        assert_eq!(
            expected, actual_trait,
            "TODO: the ref has extra junk, this should be an error"
        );
    }

    #[test]
    fn test_resolve_trait_unknown_cross_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "traits",
            camino::Utf8Path::new("./traits.alloy"),
            r"
        trait MyTrait where
        end
                ",
        );

        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
        import traits
        typeof dummy : traits::UnknownTrait
                ",
        );

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let actual_err = find_trait_error(&db, module_id);
        let expected = TypeResolutionError::UnknownTraitReference {
            source_ref,
            module_id,
            path: ne_vec!["traits".into(), "UnknownTrait".into()],
        };

        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_trait_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
        import fake_traits
        typeof dummy : fake_traits::UnknownTrait
                ",
        );

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let actual_err = find_trait_error(&db, module_id);
        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::TypeReference(source_ref),
            module_slug: "fake_traits".to_string(),
        };

        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_trait_bounded_type_reference() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typedef MyType = MyType Int
    trait MyTrait where
    end
    behavior MyTrait[t] for MyType where
        typevar t
    end
            ",
        );

        let bounded_type_idx = Idx::from_raw(RawIdx::from_u32(3));

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let type_ref = hir_module.get_type_reference(bounded_type_idx);
        assert!(matches!(type_ref, hir::TypeReference::Bounded { .. }));

        let err = resolve_trait_by_ref_id(&db, module_id, bounded_type_idx)
            .expect_err("must fail with bounded trait ref");
        let expected = TypeResolutionError::BoundedTraitReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(3)),
            },
            target_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(1)),
            },
        };

        assert_eq!(expected, err);
    }
}
