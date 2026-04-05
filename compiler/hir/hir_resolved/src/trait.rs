use crate::resolver::resolve_by_path;
use crate::{resolver, EPTrFql, Expression, Fql, HirResolutionError};
use alloy_hir_def as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;

pub struct Trait {}

/// Collect a trait and all its supertraits (transitively), returning
/// the full set of trait FQLs that are implied by this trait.
///
/// This is a Salsa tracked query so it's cached per (module, trait) pair.
#[salsa::tracked]
pub fn resolve_super_traits(
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
    trait_idx: hir::TraitIdx,
) -> Vec<Fql<hir::Trait>> {
    let trait_fql = Fql::new(module_id, trait_idx);
    let mut result = vec![trait_fql];

    let (hir_module, _) = hir::lower_file(db, module_id);
    let trait_def = hir_module.get_trait(trait_idx);
    for constraint in trait_def.self_constraints() {
        if let hir::TypeVariableConstraint::Trait(type_idx) = constraint {
            if let Ok(super_fql) = resolve_trait_by_ref_id(db, module_id, *type_idx) {
                for fql in resolve_super_traits(db, super_fql.module_id, super_fql.local_id) {
                    if !result.contains(&fql) {
                        result.push(fql);
                    }
                }
            }
        }
    }
    result
}

#[salsa::tracked]
pub fn resolve_trait_by_ref_id(
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> Result<Fql<hir::Trait>, HirResolutionError> {
    let source_ref = Fql::new(module_id, type_idx);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Named(path) => {
            resolve_by_path::<hir::Trait, TraitResolver>(db, module_id, path, source_ref)
        }
        hir::TypeReference::Bounded { base, .. } => {
            Err(HirResolutionError::BoundedTraitReference {
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

pub(crate) fn resolve_abstract_trait_member_by_path(
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
    path: &hir::Path,
    source_ref: &Fql<hir::Expression>,
) -> Option<Expression> {
    if path.has_resolution_kind(hir::ResolutionKind::AbstractTraitMember) {
        if let hir::Path::ThisModule { name, scope, .. } = path {
            let (hir_module, _) = hir::lower_file(db, module_id);

            if let Some((trait_idx, trait_def)) = hir_module.find_trait_containing_scope(*scope) {
                if let Some((member_name, type_annotation_idx)) = trait_def.abstract_member(name) {
                    return Some(Expression::AbstractTraitMemberRef {
                        trait_fql: Fql::new(module_id, trait_idx),
                        member_name: member_name.clone(),
                        type_annotation: Fql::new(module_id, type_annotation_idx),
                    });
                }
            }
        }
    }

    if path.has_resolution_kind(hir::ResolutionKind::Trait) {
        let trait_fql =
            resolve_by_path::<hir::Trait, TraitResolver>(db, module_id, path, source_ref).ok()?;
        let (hir_module, _) = hir::lower_file(db, trait_fql.module_id);

        let member_name = path.subpath()?;

        if let Some((_, type_annotation)) = hir_module
            .get_trait(trait_fql.local_id)
            .abstract_member(&member_name)
        {
            return Some(Expression::AbstractTraitMemberRef {
                trait_fql: trait_fql.clone(),
                member_name,
                type_annotation: Fql::new(trait_fql.module_id, type_annotation),
            });
        }
    }
    None
}

// ============================================================================
// Trait Resolver
// ============================================================================

struct TraitResolver;

impl resolver::Resolver<hir::Trait> for TraitResolver {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        _scope: ScopeIdx,
    ) -> Option<(Idx<hir::Trait>, hir::Trait)> {
        hir_module
            .get_trait_by_name(name)
            .map(|(idx, item)| (idx, item.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> HirResolutionError {
        HirResolutionError::UnknownTraitReference {
            source_ref: source_ref.into(),
            module_id,
            path,
        }
    }

    fn validate(
        _db: &dyn hir::HirDefDatabase,
        source_ref: impl Into<EPTrFql>,
        trait_fql: Fql<hir::Trait>,
        subname: Option<hir::Name>,
    ) -> Option<HirResolutionError> {
        if let Some(subname) = subname {
            let (hir_module, _) = hir::lower_file(_db, trait_fql.module_id);
            if hir_module
                .get_trait(trait_fql.local_id)
                .abstract_member(&subname)
                .is_some()
            {
                return None;
            }

            return Some(HirResolutionError::UnknownTraitMember {
                source_ref: source_ref.into(),
                module_id: trait_fql.module_id,
                trait_idx: trait_fql.local_id,
                subname,
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use alloy_test_harness::idx;
    use alloy_workspace::WorkspaceDatabase;
    use non_empty_vec::ne_vec;

    fn find_trait(db: &dyn hir::HirDefDatabase, module_id: ModuleId) -> Fql<hir::Trait> {
        resolve_trait_by_ref_id(db, module_id, idx!(0)).expect("must find trait")
    }

    fn find_trait_error(db: &dyn hir::HirDefDatabase, module_id: ModuleId) -> HirResolutionError {
        resolve_trait_by_ref_id(db, module_id, idx!(0)).expect_err("must fail to find trait")
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
        let expected = Fql::new(module_id, idx!(0));

        assert_eq!(expected, actual_trait);
    }

    #[test]
    fn test_resolve_trait_this_module_extra_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    trait MyTrait where
    end
    
    typeof dummy : MyTrait::extra_junk
            ",
        );

        let actual_err = find_trait_error(&db, module_id);

        let source_ref = Fql::new(module_id, idx!(0));
        let expected = HirResolutionError::UnknownTraitMember {
            source_ref: EPTrFql::TypeReference(source_ref),
            module_id: ModuleId::new(&db, "test"),
            trait_idx: idx!(0),
            subname: hir::Name::new("extra_junk"),
        };

        assert_eq!(expected, actual_err);
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

        let source_ref = Fql::new(module_id, idx!(0));
        let expected = HirResolutionError::UnknownTraitReference {
            source_ref: EPTrFql::TypeReference(source_ref),
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
        let expected = Fql::new(other_module_id, idx!(0));

        assert_eq!(expected, actual_trait);
    }

    #[test]
    fn test_resolve_trait_cross_module_extra_path() {
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
    import traits::MyTrait
    typeof dummy : MyTrait::extra_junk
            ",
        );

        let actual_err = find_trait_error(&db, module_id);

        let source_ref = Fql::new(module_id, idx!(0));
        let expected = HirResolutionError::UnknownTraitMember {
            source_ref: EPTrFql::TypeReference(source_ref),
            module_id: ModuleId::new(&db, "traits"),
            trait_idx: idx!(0),
            subname: hir::Name::new("extra_junk"),
        };

        assert_eq!(expected, actual_err);
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

        let source_ref = Fql::new(module_id, idx!(0));
        let actual_err = find_trait_error(&db, module_id);
        let expected = HirResolutionError::UnknownTraitReference {
            source_ref: EPTrFql::TypeReference(source_ref),
            module_id: ModuleId::new(&db, "traits"),
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

        let actual_err = find_trait_error(&db, module_id);

        let source_ref = Fql::new(module_id, idx!(0));
        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownRootModule {
                attempted_module_path: ne_vec![hir::Name::new("fake_traits")],
            },
            source_ref: EPTrFql::TypeReference(source_ref),
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

        let bounded_type_idx = idx!(3);

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let type_ref = hir_module.get_type_reference(bounded_type_idx);
        assert!(matches!(type_ref, hir::TypeReference::Bounded { .. }));

        let err = resolve_trait_by_ref_id(&db, module_id, bounded_type_idx)
            .expect_err("must fail with bounded trait ref");
        let expected = HirResolutionError::BoundedTraitReference {
            source_ref: Fql {
                module_id,
                local_id: idx!(3),
            },
            target_ref: Fql {
                module_id,
                local_id: idx!(1),
            },
        };

        assert_eq!(expected, err);
    }
}
