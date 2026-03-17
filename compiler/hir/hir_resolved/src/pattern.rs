use crate::type_definition::resolve_type_definition_by_path_variant;
use crate::{Fql, HirResolutionError};
use alloy_hir_def as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    Literal(hir::Literal),
    VariableDeclaration,
    Nil,
    DataDestructure {
        target: Fql<hir::TypeDefinition>,
        args: Vec<Fql<hir::Pattern>>,
    },
    VariantDestructure {
        target: Fql<hir::TypeDefinition>,
        variant_name: hir::Name,
        args: Vec<Fql<hir::Pattern>>,
    },
    Unit,
    Tuple(NonEmpty<Fql<hir::Pattern>>),
}

#[salsa::tracked]
pub fn resolve_pattern_by_id(
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
    pat_id: hir::PatternIdx,
) -> Result<Pattern, HirResolutionError> {
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
    db: &dyn hir::HirDefDatabase,
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
    db: &dyn hir::HirDefDatabase,
    source_ref: &Fql<hir::Pattern>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &[hir::PatternIdx],
) -> Result<Pattern, HirResolutionError> {
    let (type_def_fql, variant_name) =
        resolve_type_definition_by_path_variant(db, module_id, target, source_ref)?;

    let fql_args = args
        .iter()
        .map(|p| Fql::new(module_id, *p))
        .collect::<Vec<_>>();

    let Some(variant_name) = variant_name else {
        return Ok(Pattern::DataDestructure {
            target: type_def_fql,
            args: fql_args,
        });
    };

    Ok(Pattern::VariantDestructure {
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
    use alloy_test_harness::idx;
    use alloy_workspace::WorkspaceDatabase;
    use non_empty_vec::ne_vec;

    #[test]
    fn test_destructure_pattern_resolves_from_variant_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    import std::option::Option
    let unwrap = |Option::Some(x)| -> x
            ",
        );

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);

        let actual_1 =
            resolve_pattern_by_id(&db, module_id, idx!(1)).expect("expected to resolve pattern");
        assert_eq!(
            Pattern::VariantDestructure {
                target: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: idx!(0),
                },
                variant_name: hir::Name::new("Some"),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
                }],
            },
            actual_1
        );
    }

    #[test]
    fn test_destructure_pattern_resolves_from_data_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
    typedef Point = Point(Int, Int)
    let unwrap = |Point(x, y)| -> x + y
            ",
        );

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);

        let actual_1 =
            resolve_pattern_by_id(&db, module_id, idx!(1)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_1);

        let actual_2 =
            resolve_pattern_by_id(&db, module_id, idx!(2)).expect("expected to resolve pattern");
        assert_eq!(
            Pattern::DataDestructure {
                target: Fql {
                    module_id: ModuleId::new(&db, "test"),
                    local_id: idx!(0),
                },
                args: vec![
                    Fql {
                        module_id,
                        local_id: idx!(0),
                    },
                    Fql {
                        module_id,
                        local_id: idx!(1),
                    }
                ],
            },
            actual_2,
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

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, idx!(1))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            HirResolutionError::UnknownTypeDefinitionVariant {
                source_ref: EPTrFql::Pattern(Fql {
                    module_id,
                    local_id: idx!(1),
                }),
                target_type_fql: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: idx!(0),
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

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, idx!(1))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            HirResolutionError::MissingTypeDefinitionVariant {
                source_ref: EPTrFql::Pattern(Fql {
                    module_id,
                    local_id: idx!(1),
                }),
                target_type_fql: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: idx!(0),
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

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, idx!(1))
            .expect_err("expected to resolve pattern");
        assert_eq!(
            HirResolutionError::UnknownPatternReference {
                source_ref: Fql {
                    module_id,
                    local_id: idx!(1),
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

        let actual_0 =
            resolve_pattern_by_id(&db, module_id, idx!(0)).expect("expected to resolve pattern");
        assert_eq!(Pattern::VariableDeclaration, actual_0);
        let err = resolve_pattern_by_id(&db, module_id, idx!(1))
            .expect_err("expected to resolve pattern");

        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownRootModule {
                attempted_module_path: ne_vec![hir::Name::new("unknown")],
            },
            source_ref: EPTrFql::Pattern(Fql {
                module_id,
                local_id: idx!(1),
            }),
        };
        assert_eq!(expected, err);
    }
}
