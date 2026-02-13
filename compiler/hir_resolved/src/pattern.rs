use crate::type_definition::resolve_type_definition_by_path_variant;
use crate::{Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    Literal(hir::Literal),
    VariableDeclaration,
    Nil,
    Destructure {
        target: Fql<hir::TypeDefinition>,
        variant_name: hir::Name,
        args: Vec<Fql<hir::Pattern>>,
    },
    Unit,
    Tuple(NonEmpty<Fql<hir::Pattern>>),
}

#[salsa::tracked]
pub fn resolve_pattern_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    pat_id: hir::PatternIdx,
) -> Result<Pattern, TypeResolutionError> {
    let source_ref = Fql::new(module_id, pat_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let pat = hir_module.get_pattern(pat_id);

    let pat = match pat {
        hir::Pattern::Literal(lit) => Pattern::Literal(lit.clone()),
        hir::Pattern::Unit => Pattern::Unit,
        hir::Pattern::VariableDeclaration { .. } => Pattern::VariableDeclaration,
        hir::Pattern::Tuple(elements) => {
            let fql_elements = elements.iter().map(|e| Fql::new(module_id, *e)).collect();
            unsafe { Pattern::Tuple(NonEmpty::new_unchecked(fql_elements)) }
        }
        hir::Pattern::Destructure { target, args, .. } => {
            resolve_destructure(db, &source_ref, module_id, target, args)?
        }
        hir::Pattern::Nil => Pattern::Nil,
        hir::Pattern::Missing => Pattern::Missing,
    };

    Ok(pat)
}

pub(crate) fn resolve_pattern_by_path(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::Pattern>> {
    if let hir::Path::ThisModule { name, scope, .. } = path {
        let (hir_module, _) = hir::lower_file(db, module_id);
        let (var_id, _) = hir_module.get_pattern_by_name(name, *scope)?;
        Some(Fql::new(module_id, var_id))
    } else {
        None
    }
}

fn resolve_destructure(
    db: &dyn hir::HirDatabase,
    source_ref: &Fql<hir::Pattern>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &[hir::PatternIdx],
) -> Result<Pattern, TypeResolutionError> {
    let (type_def_fql, variant_name) =
        resolve_type_definition_by_path_variant(db, module_id, target, source_ref)?;

    let fql_args = args
        .iter()
        .map(|p| Fql::new(module_id, *p))
        .collect::<Vec<_>>();

    let Some(variant_name) = variant_name else {
        return Err(TypeResolutionError::MissingTypeDefinitionVariant {
            source_ref: source_ref.into(),
            target_type_fql: type_def_fql,
        });
    };

    Ok(Pattern::Destructure {
        target: type_def_fql,
        variant_name,
        args: fql_args,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use crate::EPTrFql;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::{Idx, RawIdx};
    use non_empty_vec::ne_vec;

    #[test]
    fn test_destructure_pattern_resolves() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import std::option::Option
    let unwrap = |Option::Some(x)| -> x
            ",
        );

        let actual_0 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);

        let actual_1 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect("expected to resolve pattern");
        assert_eq!(
            Pattern::Destructure {
                target: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                },
                variant_name: hir::Name::new("Some"),
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }],
            },
            actual_1
        );
    }

    #[test]
    fn test_destructure_pattern_with_invalid_variant() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import std::option::Option
    let unwrap = |Option::InvalidVariant(x)| -> x
            ",
        );

        let actual_0 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            TypeResolutionError::UnknownTypeDefinitionVariant {
                source_ref: EPTrFql::Pattern(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }),
                target_type_fql: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                },
                variant_name: hir::Name::new("InvalidVariant")
            },
            err
        );
    }

    #[test]
    fn test_destructure_pattern_with_missing_variant() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import std::option::Option
    let unwrap = |Option(x)| -> x
            ",
        );

        let actual_0 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            TypeResolutionError::MissingTypeDefinitionVariant {
                source_ref: EPTrFql::Pattern(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }),
                target_type_fql: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }
            },
            err
        );
    }

    #[test]
    fn test_destructure_pattern_with_unknown_type() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    let unwrap = |UnknownType(x)| -> x
            ",
        );

        let actual_0 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            TypeResolutionError::UnknownPatternReference {
                source_ref: Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                },
                module_id,
                path: ne_vec![hir::Name::new("UnknownType")],
            },
            err
        );
    }

    #[test]
    fn test_destructure_pattern_with_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import unknown::Option
    let unwrap = |Option::Some(x)| -> x
            ",
        );

        let actual_0 = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            TypeResolutionError::UnknownModule {
                source_ref: EPTrFql::Pattern(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }),
                module_slug: "unknown".to_string(),
            },
            err
        );
    }
}
