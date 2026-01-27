use crate::{cross_module_resolver, EPTrFql, Expression, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
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
        _module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> TypeResolutionError {
        let EPTrFql::TypeReference(source_ref) = source_ref.into() else {
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

/// Resolve a cross-module trait reference
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::RawIdx;

    fn find_trait(db: &dyn hir::HirDatabase, module_id: ModuleId, name: &str) -> Fql<hir::Trait> {
        let path = hir::Path::ThisModule {
            name: name.into(),
            subname: None,
            scope: alloy_scope::Scopes::ROOT,
        };

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        resolve_trait_by_path(db, module_id, &path, source_ref).expect("must find trait")
    }

    fn find_trait_error(
        db: &dyn hir::HirDatabase,
        module_id: ModuleId,
        name: &str,
    ) -> TypeResolutionError {
        let path = hir::Path::ThisModule {
            name: name.into(),
            subname: None,
            scope: alloy_scope::Scopes::ROOT,
        };

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        resolve_trait_by_path(db, module_id, &path, source_ref)
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
            ",
        );

        let actual = find_trait(&db, module_id, "MyTrait");
        let expected = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_resolve_trait_unknown_this_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module("test", camino::Utf8Path::new("./test.alloy"), r"");

        let actual = find_trait_error(&db, module_id, "UnknownTrait");

        let source_ref = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let expected = TypeResolutionError::UnknownTraitReference {
            source_ref,
            module_id,
            path: ne_vec!["UnknownTrait".into()],
        };

        assert_eq!(expected, actual);
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
            ",
        );

        let fqn = hir::Fqn {
            module: ne_vec![hir::Name::new("traits")],
            name: hir::Name::new("MyTrait"),
            sub_path: None,
        };

        let type_ref_fql = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let actual = resolve_cross_module_trait(&db, &fqn, type_ref_fql).expect("must find trait");
        let expected = Fql::new(other_module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual);
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
                ",
        );

        let fqn = hir::Fqn {
            module: ne_vec![hir::Name::new("traits")],
            name: hir::Name::new("UnknownTrait"),
            sub_path: None,
        };

        let type_ref_fql = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let actual = resolve_cross_module_trait(&db, &fqn, type_ref_fql.clone())
            .expect_err("must fail to find trait");
        let expected = TypeResolutionError::UnknownTraitName {
            source_ref: type_ref_fql,
            fqn,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_resolve_trait_unknown_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module("test", camino::Utf8Path::new("./test.alloy"), r"");

        let full_path = ne_vec![hir::Name::new("unknown"), hir::Name::new("path")];
        let unknown_path = hir::Path::Unknown(full_path.clone());

        let type_ref_fql = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));
        let actual = resolve_trait_by_path(&db, module_id, &unknown_path, type_ref_fql.clone())
            .expect_err("must fail to find trait");
        let expected = TypeResolutionError::UnknownTraitReference {
            source_ref: type_ref_fql,
            module_id,
            path: full_path,
        };

        assert_eq!(expected, actual);
    }
    //
    //     #[test]
    //     fn test_resolve_trait_bounded_type_reference() {
    //         let mut db = TestHirResDatabase::new_with_stdlib();
    //         let module_id = db.add_module(
    //             "test",
    //             camino::Utf8Path::new("./test.alloy"),
    //             r"
    //     trait MyTrait where
    //     end
    //             ",
    //         );
    //
    //         let (hir_module, _) = hir::lower_file(&db, module_id);
    //         // Create a bounded type reference scenario
    //         let type_refs: Vec<_> = hir_module.type_references()
    //             .map(|(idx, _, _, _)| idx)
    //             .collect();
    //
    //         for type_ref_idx in type_refs {
    //             let type_ref = hir_module.get_type_reference(type_ref_idx);
    //             if let hir::TypeReference::Bounded { .. } = type_ref {
    //                 let result = resolve_trait_by_ref_id(&db, module_id, type_ref_idx);
    //                 assert!(result.is_err(), "Should fail with bounded type reference");
    //
    //                 if let Err(TypeResolutionError::BoundedTraitReference { .. }) = result {
    //                     // Correct error type
    //                     return;
    //                 } else {
    //                     panic!("Expected BoundedTraitReference error");
    //                 }
    //             }
    //         }
    //     }
    //
    //     #[test]
    //     fn test_error_get_range_for_trait_errors() {
    //         let mut db = TestHirResDatabase::new_with_stdlib();
    //         let module_id = db.add_module(
    //             "test",
    //             camino::Utf8Path::new("./test.alloy"),
    //             r"
    //     behavior UnknownTrait for Int where
    //     end
    //             ",
    //         );
    //
    //         let path = hir::Path::ThisModule {
    //             name: hir::Name::new("UnknownTrait"),
    //             subname: None,
    //             scope: alloy_scope::Scopes::ROOT,
    //         };
    //
    //         let type_ref_fql = Fql::new(module_id, la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(0)));
    //         if let Err(err) = resolve_trait_by_path(&db, module_id, &path, type_ref_fql) {
    //             // Verify get_range() works for trait errors
    //             let range = err.get_range(&db);
    //             assert!(range.len() > 0.into());
    //         }
    //     }
}
