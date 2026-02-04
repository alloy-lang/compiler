use crate::{resolver, EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;

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
    source_ref: impl Into<EPTrFql> + Clone,
) -> Result<(Fql<hir::TypeDefinition>, hir::Name), TypeResolutionError> {
    let variant_name = match path {
        hir::Path::ThisModule { subname, .. } => subname.clone(),
        hir::Path::OtherModule(fqn) => fqn.sub_path.clone(),
        hir::Path::Unknown(_) => None,
    };

    let type_def_fql = resolver::resolve_by_path::<hir::TypeDefinition, TypeDefinitionResolver>(
        db,
        module_id,
        path,
        source_ref.clone(),
    )?;

    let Some(variant_name) = variant_name else {
        return Err(TypeResolutionError::MissingTypeDefinitionVariant {
            source_ref: source_ref.into(),
            target_type_fql: type_def_fql,
        });
    };

    Ok((type_def_fql, variant_name))
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
        hir::TypeReference::Named(path) => resolver::resolve_by_path::<
            hir::TypeDefinition,
            TypeDefinitionResolver,
        >(db, module_id, path, source_ref),
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

// ============================================================================
// Type Definition Resolver
// ============================================================================

struct TypeDefinitionResolver;

impl resolver::Resolver<hir::TypeDefinition> for TypeDefinitionResolver {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        scope: ScopeIdx,
    ) -> Option<(Idx<hir::TypeDefinition>, hir::TypeDefinition)> {
        hir_module
            .get_type_definition_by_name(name, scope)
            .map(|(id, typedef)| (id, typedef.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> TypeResolutionError {
        match source_ref.into() {
            EPTrFql::Expression(fql) => TypeResolutionError::UnknownExpressionReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::Pattern(fql) => TypeResolutionError::UnknownPatternReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::TypeReference(fql) => TypeResolutionError::UnknownTypeDefinition {
                source_ref: fql,
                module_id,
                path,
            },
        }
    }

    fn validate(
        db: &dyn hir::HirDatabase,
        source_ref: impl Into<EPTrFql>,
        type_def_fql: Fql<hir::TypeDefinition>,
        subname: Option<hir::Name>,
    ) -> Option<TypeResolutionError> {
        // For type definitions, check if the variant exists (if one is requested)
        if let Some(variant_name) = &subname {
            let (hir_module, _) = hir::lower_file(db, type_def_fql.module_id);
            let type_def = hir_module.get_type_definition(type_def_fql.local_id);
            if !type_def.kind.has_variant(variant_name) {
                return Some(TypeResolutionError::UnknownTypeDefinitionVariant {
                    source_ref: source_ref.into(),
                    target_type_fql: type_def_fql,
                    variant_name: variant_name.clone(),
                });
            }
        }
        None
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
    use non_empty_vec::ne_vec;

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
            variant_name: hir::Name::new("Extra"),
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
            variant_name: hir::Name::new("Extra"),
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
