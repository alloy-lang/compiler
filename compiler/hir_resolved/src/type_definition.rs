use crate::{cross_module_resolver, EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use cross_module_resolver::resolve_cross_module_optional;
use la_arena::Idx;
use non_empty_vec::{ne_vec, NonEmpty};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: hir::Name,
    pub kind: TypeDefinitionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinitionKind {
    Single(TypeDefinitionMember),
    Union(Vec<TypeDefinitionMember>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinitionMember {
    name: hir::Name,
    properties: Vec<Fql<hir::TypeReference>>,
}

impl TypeDefinitionMember {
    pub fn name(&self) -> &hir::Name {
        &self.name
    }

    pub fn properties(&self) -> &[Fql<hir::TypeReference>] {
        &self.properties
    }
}

pub(crate) fn resolve_type_definition_by_path_variant(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
) -> Option<(Fql<hir::TypeDefinition>, hir::Name)> {
    let (type_def_fql, variant_name): (Fql<hir::TypeDefinition>, hir::Name) = match path {
        hir::Path::ThisModule {
            name,
            subname,
            scope,
        } => {
            let variant_name = subname.clone()?;
            let type_def_id = get_type_definition_by_name(db, module_id, name, *scope)?;

            (type_def_id, variant_name)
        }
        hir::Path::OtherModule(fqn) => {
            let variant_name = fqn.sub_path.clone()?;
            let td_fql = resolve_cross_module_optional::<hir::TypeDefinition, TypeDefinitionLookup>(
                db, fqn,
            )?;

            (td_fql, variant_name)
        }
        hir::Path::Unknown(_) => return None,
    };

    Some((type_def_fql, variant_name))
}

#[salsa::tracked]
pub fn resolve_type_definition_by_ref_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    let source_ref = Fql::new(module_id, type_idx);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Named(path) => {
            resolve_type_definition_by_path(db, module_id, path, source_ref)
        }
        hir::TypeReference::Bounded { base, args: _ } => {
            // TODO: bounded type reference should check args
            resolve_type_definition_by_ref_id(db, module_id, *base)
        }
        _ => unreachable!(
            "Invalid type reference for trait resolution: {:?}",
            type_ref
        ),
    }
}

#[salsa::tracked]
pub fn resolve_type_definition_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    type_def_idx: hir::TypeDefinitionIdx,
) -> Option<TypeDefinition> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_def = hir_module.get_type_definition(type_def_idx);

    let kind = match &type_def.kind {
        hir::TypeDefinitionKind::Single(member) => {
            let resolved_member = TypeDefinitionMember {
                name: member.name().clone(),
                properties: member
                    .properties()
                    .iter()
                    .map(|type_ref_idx| Fql::new(module_id, *type_ref_idx))
                    .collect(),
            };
            TypeDefinitionKind::Single(resolved_member)
        }
        hir::TypeDefinitionKind::Union(members) => {
            let resolved_members = members
                .iter()
                .map(|member| TypeDefinitionMember {
                    name: member.name().clone(),
                    properties: member
                        .properties()
                        .iter()
                        .map(|type_ref_idx| Fql::new(module_id, *type_ref_idx))
                        .collect(),
                })
                .collect();
            TypeDefinitionKind::Union(resolved_members)
        }
        hir::TypeDefinitionKind::Missing | hir::TypeDefinitionKind::TypeVariable(_) => {
            return None; // or handle as needed
        }
    };

    Some(TypeDefinition {
        name: type_def.name.clone(),
        kind,
    })
}

fn resolve_type_definition_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    source_ref: Fql<hir::TypeReference>,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    if let Some(type_def_fql) = resolve_type_definition_by_path_op(db, current_module_id, path) {
        return Ok(type_def_fql);
    }

    let (error_module_id, error_path) = match path {
        hir::Path::ThisModule { name, .. } => (current_module_id, ne_vec![name.clone()]),
        hir::Path::OtherModule(fqn) => {
            let Some(module_id) = db.find_module_by_slug(&fqn.module_slug()) else {
                return Err(TypeResolutionError::UnknownModule {
                    module_slug: fqn.module_slug(),
                    source_ref: source_ref.into(),
                });
            };

            (module_id, fqn.segments())
        }
        hir::Path::Unknown(names) => (current_module_id, names.clone()),
    };

    Err(TypeResolutionError::UnknownTypeDefinition {
        source_ref,
        module_id: error_module_id,
        path: error_path,
    })
}

fn resolve_type_definition_by_path_op(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::TypeDefinition>> {
    match path {
        hir::Path::ThisModule { name, scope, .. } => {
            get_type_definition_by_name(db, current_module_id, name, *scope)
        }
        hir::Path::OtherModule(fqn) => {
            resolve_cross_module_optional::<hir::TypeDefinition, TypeDefinitionLookup>(db, fqn)
        }
        hir::Path::Unknown(names) => None,
    }
}

fn get_type_definition_by_name(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::TypeDefinition>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let (type_idx, _) = hir_module.get_type_definition_by_name(name, scope)?;

    Some(Fql::new(module_id, type_idx))
}

// ============================================================================
// Type Definition Lookup
// ============================================================================

struct TypeDefinitionLookup;

impl cross_module_resolver::ModuleLookup<hir::TypeDefinition> for TypeDefinitionLookup {
    type Item = hir::TypeDefinition;

    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<hir::TypeDefinition>, Self::Item)> {
        hir_module
            .get_type_definition_by_name(name, Scopes::ROOT)
            .map(|(id, typedef)| (id, typedef.clone()))
    }

    fn validate(item: Self::Item, remaining_path: &[hir::Name]) -> bool {
        // For type definitions, check if the variant exists (if one is requested)
        if let Some(variant_name) = remaining_path.last() {
            return item.kind.has_variant(variant_name);
        }
        true
    }

    fn validation_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        _path: NonEmpty<hir::Name>,
        remaining_path: &[hir::Name],
        item_id: hir::TypeDefinitionIdx,
    ) -> TypeResolutionError {
        TypeResolutionError::UnknownTypeDefinitionVariant {
            source_ref: source_ref.into(),
            target_type_fql: Fql {
                module_id,
                local_id: item_id,
            },
            variant_name: remaining_path.iter().next().cloned(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use crate::EPTrFql;
    use alloy_hir::TypeIdx;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::{Idx, RawIdx};

    const TYPE_REF_IDX: TypeIdx = Idx::from_raw(RawIdx::from_u32(0));

    #[test]
    fn test_resolve_type_definition_this_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typedef MyType = Thing
    typeof dummy : MyType
            ",
        );

        let actual_ref = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect("must find type def");
        let expected = Fql::new(module_id, Idx::from_raw(RawIdx::from_u32(0)));

        assert_eq!(expected, actual_ref);
    }

    #[test]
    fn test_resolve_type_definition_this_module_extra_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typedef MyType = Thing
    typeof dummy : MyType::Extra
            ",
        );

        let actual_err = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect_err("must fail to find type def variant");
        let expected = TypeResolutionError::UnknownTypeDefinitionVariant {
            source_ref: EPTrFql::TypeReference(Fql {
                module_id,
                local_id: TYPE_REF_IDX,
            }),
            target_type_fql: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            variant_name: Some(hir::Name::new("Extra")),
        };

        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_type_definition_cross_module() {
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

        let actual_ref = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect("must find type def");
        let expected = Fql::new(
            ModuleId::new(&db, "types"),
            Idx::from_raw(RawIdx::from_u32(0)),
        );

        assert_eq!(expected, actual_ref);
    }

    #[test]
    fn test_resolve_type_definition_cross_module_extra_path() {
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
    typeof dummy : types::MyType::Extra
            ",
        );

        let actual_err = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect_err("must fail to find type def variant");
        let expected = TypeResolutionError::UnknownTypeDefinitionVariant {
            source_ref: EPTrFql::TypeReference(Fql {
                module_id,
                local_id: TYPE_REF_IDX,
            }),
            target_type_fql: Fql {
                module_id: ModuleId::new(&db, "types"),
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            variant_name: Some(hir::Name::new("Extra")),
        };

        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_type_definition_cross_module_returns_error_for_unknown_type() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let other_module_id = db.add_module(
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

        let actual_err = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect_err("must not find type def");
        let expected = TypeResolutionError::UnknownTypeDefinition {
            source_ref: Fql {
                module_id,
                local_id: TYPE_REF_IDX,
            },
            module_id: other_module_id,
            path: ne_vec![hir::Name::new("types"), hir::Name::new("UnknownType")],
        };
        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_type_definition_cross_module_returns_err_for_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import fake
    typeof dummy : fake::TotallyKnownType
            ",
        );

        let actual_err = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect_err("must not find type def");
        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::TypeReference(Fql {
                module_id,
                local_id: TYPE_REF_IDX,
            }),
            module_slug: "fake".to_string(),
        };
        assert_eq!(expected, actual_err);
    }

    #[test]
    fn test_resolve_type_definition_unknown_path() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typeof foo : unknown::path::Type
            ",
        );

        let actual_err = resolve_type_definition_by_ref_id(&db, module_id, TYPE_REF_IDX)
            .expect_err("must not find type def");
        let expected = TypeResolutionError::UnknownTypeDefinition {
            source_ref: Fql {
                module_id,
                local_id: TYPE_REF_IDX,
            },
            module_id,
            path: ne_vec![hir::Name::new("unknown"), hir::Name::new("path")],
        };
        assert_eq!(expected, actual_err);
    }
}
