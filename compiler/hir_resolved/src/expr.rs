use super::{cross_module_resolver, EPTdFql, ExpressionLookup, TypeDefinitionLookup};
use crate::diagnostics::TypeResolutionError;
use crate::pattern::resolve_pattern_by_path;
use crate::r#trait::resolve_abstract_trait_member_by_path;
use crate::type_definition::resolve_type_definition_by_path_variant;
use crate::{EPFql, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use cross_module_resolver::resolve_cross_module_optional;
use non_empty_vec::{ne_vec, NonEmpty};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Missing,
    Literal(hir::Literal),
    VariableRef(EPFql),
    Binary {
        op: hir::BinaryOp,
        lhs: Fql<hir::Expression>,
        rhs: Fql<hir::Expression>,
    },
    Unit,
    IfThenElse {
        condition: Fql<hir::Expression>,
        then: Fql<hir::Expression>,
        else_: Fql<hir::Expression>,
    },
    Tuple(NonEmpty<Fql<hir::Expression>>),
    Unary {
        op: hir::UnaryOp,
        expression: Fql<hir::Expression>,
    },
    Lambda {
        args: Vec<Fql<hir::Pattern>>,
        body: Fql<hir::Expression>,
    },
    FunctionCall {
        target: EPTdFql,
        variant_name: Option<hir::Name>,
        args: Vec<Fql<hir::Expression>>,
    },
    Match {
        condition: Fql<hir::Expression>,
        targets: Vec<(Fql<hir::Pattern>, Fql<hir::Expression>)>,
    },
    /// Reference to a variant constructor (e.g., Option::None or Option::Some)
    VariantConstructor {
        type_def: Fql<hir::TypeDefinition>,
        variant_name: hir::Name,
    },
    /// Reference to an abstract trait member (has type annotation but no implementation)
    AbstractTraitMemberRef {
        trait_fql: Fql<hir::Trait>,
        member_name: hir::Name,
        type_annotation: Fql<hir::TypeReference>,
    },
}

#[salsa::tracked]
pub fn resolve_expression_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) -> Result<Expression, TypeResolutionError> {
    let source_ref = Fql::new(module_id, expr_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expr = hir_module.get_expression(expr_id);

    let expr = match expr {
        hir::Expression::Literal(lit) => Expression::Literal(lit.clone()),
        hir::Expression::Unit => Expression::Unit,
        hir::Expression::VariableRef { path, .. } => {
            resolve_variable_ref(db, source_ref, module_id, path)?
        }
        hir::Expression::Lambda { args, body } => {
            let fql_args = args
                .iter()
                .map(|p| Fql::new(module_id, *p))
                .collect::<Vec<_>>();
            Expression::Lambda {
                args: fql_args,
                body: Fql::new(module_id, *body),
            }
        }
        hir::Expression::FunctionCall { target, args, .. } => {
            resolve_function_call(db, &source_ref, module_id, target, args)?
        }
        hir::Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Fql::new(module_id, *lhs),
            rhs: Fql::new(module_id, *rhs),
        },
        hir::Expression::Tuple(elements) => {
            let fql_elements = elements.iter().map(|e| Fql::new(module_id, *e)).collect();
            unsafe { Expression::Tuple(NonEmpty::new_unchecked(fql_elements)) }
        }
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => Expression::IfThenElse {
            condition: Fql::new(module_id, *condition),
            then: Fql::new(module_id, *then),
            else_: Fql::new(module_id, *else_),
        },
        hir::Expression::Unary { op, expression } => Expression::Unary {
            op: op.clone(),
            expression: Fql::new(module_id, *expression),
        },
        hir::Expression::Match { condition, targets } => Expression::Match {
            condition: Fql::new(module_id, *condition),
            targets: targets
                .iter()
                .map(|(pat, expr)| (Fql::new(module_id, *pat), Fql::new(module_id, *expr)))
                .collect(),
        },
        hir::Expression::Missing => Expression::Missing,
    };

    Ok(expr)
}

pub(crate) fn resolve_expression_by_path(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::Expression>> {
    match path {
        hir::Path::ThisModule { name, scope, .. } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            let (var_id, _) = hir_module.get_expression_by_name(name, *scope)?;
            Some(Fql::new(module_id, var_id))
        }
        hir::Path::OtherModule(fqn) => {
            resolve_cross_module_optional::<hir::Expression, ExpressionLookup>(db, fqn)
        }
        hir::Path::Unknown(_) => None,
    }
}

fn resolve_variable_ref(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    path: &hir::Path,
) -> Result<Expression, TypeResolutionError> {
    if let Some(var_fql) = resolve_expression_by_path(db, module_id, path) {
        return Ok(Expression::VariableRef(var_fql.into()));
    }
    if let Some(pat_fql) = resolve_pattern_by_path(db, module_id, path) {
        return Ok(Expression::VariableRef(pat_fql.into()));
    }
    if let Some((type_def_fql, variant_name)) =
        resolve_type_definition_by_path_variant(db, module_id, path)
    {
        return Ok(Expression::VariantConstructor {
            type_def: type_def_fql,
            variant_name,
        });
    }
    if let Some(expr) = resolve_abstract_trait_member_by_path(db, module_id, path) {
        return Ok(expr);
    }

    let error_path = match path {
        hir::Path::ThisModule { name, .. } => ne_vec![name.clone()],
        hir::Path::OtherModule(fqn) => {
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

    Err(TypeResolutionError::UnknownExpressionReference {
        source_ref,
        module_id,
        path: error_path,
    })
}

fn resolve_function_call(
    db: &dyn hir::HirDatabase,
    source_ref: &Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &[hir::ExpressionIdx],
) -> Result<Expression, TypeResolutionError> {
    let (fql, variant_name) = find_function_target(db, source_ref, module_id, target)?;

    Ok(Expression::FunctionCall {
        target: fql,
        variant_name,
        args: args
            .iter()
            .map(|arg_id| Fql::new(module_id, *arg_id))
            .collect(),
    })
}

fn find_function_target(
    db: &dyn hir::HirDatabase,
    source_ref: &Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
) -> Result<(EPTdFql, Option<hir::Name>), TypeResolutionError> {
    if let Some(var_fql) = resolve_expression_by_path(db, module_id, target) {
        return Ok((var_fql.into(), None));
    }
    if let Some(pat_fql) = resolve_pattern_by_path(db, module_id, target) {
        return Ok((pat_fql.into(), None));
    }
    if let Some((type_def_fql, variant_name)) =
        resolve_type_definition_by_path_variant(db, module_id, target)
    {
        return Ok((type_def_fql.into(), Some(variant_name)));
    }
    // TODO: function calls to abstract trait members
    // if let Some(expr) = resolve_abstract_trait_member_by_path(db, module_id, target) {
    //     return Err(Ok(expr));
    // }

    let error_path = match target {
        hir::Path::ThisModule { name, .. } => ne_vec![name.clone()],
        hir::Path::OtherModule(fqn) => {
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

    Err(TypeResolutionError::UnknownExpressionReference {
        source_ref: source_ref.clone(),
        module_id,
        path: error_path,
    })
}

#[cfg(test)]
mod tests {
    use super::{resolve_expression_by_id, Expression};
    use crate::tests::TestHirResDatabase;
    use crate::{
        resolve_pattern_by_id, EPFql, EPTdFql, EPTrFql, Fql, Pattern, TypeResolutionError,
    };
    use alloy_hir as hir;
    use alloy_hir::Name;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use la_arena::{Idx, RawIdx};
    use non_empty_vec::ne_vec;

    fn maybe_find_example(
        db: &dyn hir::HirDatabase,
        module_id: ModuleId,
    ) -> Result<Expression, TypeResolutionError> {
        let (hir_module, _) = hir::lower_file(db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), alloy_scope::Scopes::ROOT)
            .unwrap_or_else(|| panic!("expected expression. hir_module: {:#?}", hir_module));
        resolve_expression_by_id(db, module_id, idx)
    }

    fn find_example(db: &dyn hir::HirDatabase, module_id: ModuleId) -> Expression {
        let actual = maybe_find_example(db, module_id).expect("must find expression");
        let Expression::VariableRef(EPFql::Expression(fql)) = actual else {
            panic!("expected actual to be VariableRef, but was {:?}", actual);
        };

        resolve_expression_by_id(db, fql.module_id, fql.local_id).expect("must find expression")
    }

    //
    // variable_ref - this module
    //

    #[test]
    fn resolve_same_module_variable_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let test_data = 1
    let example = test_data
            ",
        );

        let actual_ref = find_example(&db, module_id);

        assert_eq!(Expression::Literal(hir::Literal::Int(1)), actual_ref);
    }

    #[test]
    fn resolve_same_module_pattern_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let thing = |arg1| -> (arg1, 0)
            ",
        );

        let actual = resolve_expression_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(0)))
            .expect("resolve expression 0");
        let Expression::VariableRef(EPFql::Pattern(fql)) = actual else {
            panic!("expected actual to be VariableRef, but was {:?}", actual);
        };

        let actual_ref =
            resolve_pattern_by_id(&db, fql.module_id, fql.local_id).expect("must find expression");

        assert_eq!(Pattern::VariableDeclaration, actual_ref);
    }

    #[test]
    fn resolve_same_module_trait_member_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    trait TestTrait1 where
        typeof abstract : Int
        let example = abstract
    end
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), Idx::from_raw(RawIdx::from_u32(1)))
            .unwrap_or_else(|| panic!("expected expression. hir_module: {:#?}", hir_module));

        let actual = resolve_expression_by_id(&db, module_id, idx).expect("must find expression");
        let Expression::AbstractTraitMemberRef { member_name, .. } = actual else {
            panic!(
                "expected actual to be AbstractTraitMemberRef, but was {:?}",
                actual
            );
        };

        assert_eq!(Name::from("abstract"), member_name);
    }

    #[test]
    fn resolve_same_module_type_def_variant_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    typedef Option[t] = Some(t) | None

    let example = Option::None
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::VariantConstructor {
                type_def: Fql {
                    module_id: ModuleId::new(&db, "test_stuff"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                },
                variant_name: Name::from("None")
            },
            actual
        );
    }

    //
    // function_call - this module
    //

    #[test]
    fn resolve_same_module_function_call_variable_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let test_func = |x| -> x + 1
    let example = test_func(123)
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::Expression(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(3)),
                }),
                variant_name: None,
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(4)),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_same_module_function_call_variant_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    typedef Option[t] = Some(t) | None
    let example = Option::Some(123)
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::TypeDefinition(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }),
                variant_name: Some(Name::new("Some")),
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_same_module_function_call_argument_reference() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let example = |func, arg| -> func(arg)
            ",
        );

        let actual = resolve_expression_by_id(&db, module_id, Idx::from_raw(RawIdx::from_u32(1)))
            .expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::Pattern(Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }),
                variant_name: None,
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }],
            },
            actual,
        );
    }

    //
    // variable_ref - cross module
    //

    #[test]
    fn resolve_cross_module_variable_ref_with_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import unknown::unknown_test_data

    let example = unknown_test_data
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must fail to find expression");

        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            }),
            module_slug: "unknown".to_string(),
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_variable_ref_with_direct_import_unknown_variable() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r"
    let test_data = 1
            ",
        );

        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import other::unknown_test_data

    let example = unknown_test_data
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must fail to find expression");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            module_id,
            path: ne_vec![Name::new("other"), Name::new("unknown_test_data")],
        };
        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_variable_ref_with_direct_import() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r"
    let test_data = 1
            ",
        );

        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import other::test_data

    let example = test_data
            ",
        );

        let actual_ref = find_example(&db, module_id);

        assert_eq!(Expression::Literal(hir::Literal::Int(1)), actual_ref);
    }

    #[test]
    fn resolve_cross_module_variable_ref_with_extended_import() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r"
    let test_data = 1
            ",
        );

        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import other

    let example = other::test_data
            ",
        );

        let actual_ref = find_example(&db, module_id);

        assert_eq!(Expression::Literal(hir::Literal::Int(1)), actual_ref);
    }

    #[test]
    fn resolve_cross_module_variable_ref_with_extra_path_returns_error() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r"
    let test_data = 1
            ",
        );

        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import other

    let example = other::test_data::stuff
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must fail to find expression");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            module_id,
            path: ne_vec![
                Name::new("other"),
                Name::new("test_data"),
                Name::new("stuff")
            ],
        };

        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_variable_ref_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import not_other

    let example = not_other::test_data
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must find module error");

        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            }),
            module_slug: "not_other".to_string(),
        };

        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_variable_ref_type_def_variant_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import std::option::Option

    let example = Option::None
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::VariantConstructor {
                type_def: Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                },
                variant_name: Name::from("None")
            },
            actual
        );
    }

    #[test]
    fn resolve_cross_module_variable_ref_type_def_variant_constructor_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import std::unknown::Unknown

    let example = Unknown::None
            ",
        );

        let err =
            maybe_find_example(&db, module_id).expect_err("must fail to find type def variant");

        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            }),
            module_slug: "std::unknown".to_string(),
        };

        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_variable_ref_type_def_with_incorrect_sub_path_returns_error() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import std::option::Option

    let example = Option::Other
            ",
        );

        let err =
            maybe_find_example(&db, module_id).expect_err("must fail to find type def variant");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            module_id,
            path: ne_vec![
                Name::new("std"),
                Name::new("option"),
                Name::new("Option"),
                Name::new("Other")
            ],
        };

        assert_eq!(expected, err);
    }

    //
    // function_call - cross module
    //

    #[test]
    fn resolve_cross_module_function_call_variable_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        db.add_module(
            "other",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let test_func = |x| -> x + 1
            ",
        );
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import other

    let example = other::test_func(123)
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::Expression(Fql {
                    module_id: ModuleId::new(&db, "other"),
                    local_id: Idx::from_raw(RawIdx::from_u32(3)),
                }),
                variant_name: None,
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_cross_module_function_call_variant_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import std::option::Option

    let example = Option::Some(123)
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::TypeDefinition(Fql {
                    module_id: ModuleId::new(&db, "std::option"),
                    local_id: Idx::from_raw(RawIdx::from_u32(1)),
                }),
                variant_name: Some(Name::new("Some")),
                args: vec![Fql {
                    module_id,
                    local_id: Idx::from_raw(RawIdx::from_u32(0)),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_cross_module_variant_constructor_type_def_with_incorrect_sub_path_returns_error() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import std::option::Option

    let example = Option::Other(123)
            ",
        );

        let err =
            maybe_find_example(&db, module_id).expect_err("must fail to find type def variant");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(1)),
            },
            module_id,
            path: ne_vec![
                Name::new("std"),
                Name::new("option"),
                Name::new("Option"),
                Name::new("Other")
            ],
        };

        assert_eq!(expected, err);
    }

    #[test]
    fn resolve_cross_module_function_call_unknown_module() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    import not_other

    let example = not_other::test_func(123)
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must find module error");

        let expected = TypeResolutionError::UnknownModule {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(1)),
            }),
            module_slug: "not_other".to_string(),
        };

        assert_eq!(expected, err);
    }

    //
    // variable_ref - unknown path
    //

    #[test]
    fn unknown_path_variable_ref() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let example = unknown
            ",
        );

        let err = maybe_find_example(&db, module_id).expect_err("must fail to find variable ref");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(0)),
            },
            module_id,
            path: ne_vec![Name::new("unknown")],
        };

        assert_eq!(expected, err);
    }

    //
    // function_call - unknown path
    //

    #[test]
    fn unknown_path_function_call() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    let example = unknown(123)
            ",
        );

        let err =
            maybe_find_example(&db, module_id).expect_err("must fail to find function target");

        let expected = TypeResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: Idx::from_raw(RawIdx::from_u32(1)),
            },
            module_id,
            path: ne_vec![Name::new("unknown")],
        };

        assert_eq!(expected, err);
    }
}
