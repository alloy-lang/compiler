use super::{resolver, EPTdFql};
use crate::diagnostics::HirResolutionError;
use crate::fql::EPTrFql;
use crate::pattern::resolve_pattern_by_path;
use crate::r#trait::resolve_abstract_trait_member_by_path;
use crate::resolver::resolve_by_path;
use crate::type_definition::resolve_type_definition_by_path_variant;
use crate::{EPFql, Fql};
use alloy_hir_def as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
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
        args: Vec<Fql<hir::Expression>>,
    },
    AbstractTraitFunctionCall {
        trait_fql: Fql<hir::Trait>,
        member_name: hir::Name,
        type_annotation: Fql<hir::TypeReference>,
        args: Vec<Fql<hir::Expression>>,
    },
    Match {
        condition: Fql<hir::Expression>,
        targets: Vec<(Fql<hir::Pattern>, Fql<hir::Expression>)>,
    },
    /// Reference to a data constructor (e.g., Identity)
    DataConstructor {
        type_def: Fql<hir::TypeDefinition>,
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
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) -> Result<Expression, HirResolutionError> {
    let source_ref = Fql::new(module_id, expr_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expr = hir_module.get_expression(expr_id);

    let expr = match expr {
        hir::Expression::Literal(lit) => Expression::Literal(lit.clone()),
        hir::Expression::Unit => Expression::Unit,
        hir::Expression::VariableRef { path, .. } => {
            resolve_variable_ref(db, &source_ref, module_id, path)?
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

fn resolve_variable_ref(
    db: &dyn hir::HirDefDatabase,
    source_ref: &Fql<hir::Expression>,
    module_id: ModuleId,
    path: &hir::Path,
) -> Result<Expression, HirResolutionError> {
    if let Ok(var_fql) =
        resolve_by_path::<hir::Expression, ExpressionResolver>(db, module_id, path, source_ref)
    {
        return Ok(Expression::VariableRef(var_fql.into()));
    }
    if let Some(pat_fql) = resolve_pattern_by_path(db, module_id, path) {
        return Ok(Expression::VariableRef(pat_fql.into()));
    }
    if let Some(expr) = resolve_abstract_trait_member_by_path(db, module_id, path, source_ref) {
        return Ok(expr);
    }
    let (type_def_fql, variant_name) =
        resolve_type_definition_by_path_variant(db, module_id, path, source_ref)?;

    let Some(variant_name) = variant_name else {
        return Ok(Expression::DataConstructor {
            type_def: type_def_fql,
        });
    };

    Ok(Expression::VariantConstructor {
        type_def: type_def_fql,
        variant_name,
    })
}

fn resolve_function_call(
    db: &dyn hir::HirDefDatabase,
    source_ref: &Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &[hir::ExpressionIdx],
) -> Result<Expression, HirResolutionError> {
    let args: Vec<_> = args
        .iter()
        .map(|arg_id| Fql::new(module_id, *arg_id))
        .collect();

    if let Some(Expression::AbstractTraitMemberRef {
        trait_fql,
        member_name,
        type_annotation,
    }) = resolve_abstract_trait_member_by_path(db, module_id, target, source_ref)
    {
        return Ok(Expression::AbstractTraitFunctionCall {
            trait_fql,
            member_name,
            type_annotation,
            args,
        });
    };

    let target = find_function_target(db, source_ref, module_id, target)?;

    Ok(Expression::FunctionCall { target, args })
}

fn find_function_target(
    db: &dyn hir::HirDefDatabase,
    source_ref: &Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
) -> Result<EPTdFql, HirResolutionError> {
    if let Ok(var_fql) =
        resolve_by_path::<hir::Expression, ExpressionResolver>(db, module_id, target, source_ref)
    {
        return Ok(var_fql.into());
    }
    if let Some(pat_fql) = resolve_pattern_by_path(db, module_id, target) {
        return Ok(pat_fql.into());
    }

    let (type_def_fql, variant_name) =
        resolve_type_definition_by_path_variant(db, module_id, target, source_ref)?;

    match variant_name {
        None => Ok(type_def_fql.into()),
        Some(variant_name) => Ok(EPTdFql::TypeDefinitionVariant(
            type_def_fql,
            variant_name.clone(),
        )),
    }
}

pub fn resolve_custom_binary_operator(
    db: &dyn hir::HirDefDatabase,
    source_ref: &Fql<hir::Expression>,
    operator_path: &hir::Path,
) -> Result<Fql<hir::Expression>, HirResolutionError> {
    resolve_by_path::<hir::Expression, ExpressionResolver>(
        db,
        source_ref.module_id,
        operator_path,
        source_ref,
    )
}

// ============================================================================
// Expression Resolver
// ============================================================================

struct ExpressionResolver;

impl resolver::Resolver<hir::Expression> for ExpressionResolver {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        scope: ScopeIdx,
    ) -> Option<(Idx<hir::Expression>, hir::Expression)> {
        hir_module
            .get_expression_by_name(name, scope)
            .map(|(id, expr)| (id, expr.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> HirResolutionError {
        let EPTrFql::Expression(source_ref) = source_ref.into() else {
            panic!("Expression resolution requires Expression");
        };

        HirResolutionError::UnknownExpressionReference {
            source_ref,
            module_id,
            path,
        }
    }

    fn validate(
        _db: &dyn hir::HirDefDatabase,
        source_ref: impl Into<EPTrFql>,
        item_fql: Fql<hir::Expression>,
        subname: Option<hir::Name>,
    ) -> Option<HirResolutionError> {
        if let Some(subname) = subname {
            let EPTrFql::Expression(source_ref) = source_ref.into() else {
                panic!("Expression resolution requires Expression");
            };

            return Some(HirResolutionError::UnknownExpressionReference {
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
    use super::{resolve_expression_by_id, Expression};
    use crate::tests::TestHirResDatabase;
    use crate::{resolve_pattern_by_id, EPFql, EPTdFql, EPTrFql, Fql, HirResolutionError, Pattern};
    use alloy_hir_def as hir;
    use alloy_hir_def::Name;
    use alloy_test_harness::idx;
    use alloy_workspace::{ModuleId, VirtualModuleId, WorkspaceDatabase};
    use non_empty_vec::ne_vec;

    fn maybe_find_example(
        db: &dyn hir::HirDefDatabase,
        module_id: ModuleId,
    ) -> Result<Expression, HirResolutionError> {
        let (hir_module, _) = hir::lower_file(db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), alloy_scope::Scopes::ROOT)
            .unwrap_or_else(|| panic!("expected expression. hir_module: {:#?}", hir_module));
        resolve_expression_by_id(db, module_id, idx)
    }

    fn find_example(db: &dyn hir::HirDefDatabase, module_id: ModuleId) -> Expression {
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

        let actual =
            resolve_expression_by_id(&db, module_id, idx!(0)).expect("resolve expression 0");
        let Expression::VariableRef(EPFql::Pattern(fql)) = actual else {
            panic!("expected actual to be VariableRef, but was {:?}", actual);
        };

        let actual_ref =
            resolve_pattern_by_id(&db, fql.module_id, fql.local_id).expect("must find expression");

        assert_eq!(Pattern::VariableDeclaration, actual_ref);
    }

    #[test]
    fn resolve_same_module_trait_member_ref_inside_trait() {
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
            .get_expression_by_name(&Name::new("example"), idx!(1))
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
    fn resolve_same_module_trait_member_ref_outside_trait() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    trait TestTrait1 where
        typeof abstract : self -> Int
    end

    let example = TestTrait1::abstract
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), idx!(1))
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
                    local_id: idx!(0),
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
                    local_id: idx!(3),
                }),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(4),
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
                target: EPTdFql::TypeDefinitionVariant(
                    Fql {
                        module_id,
                        local_id: idx!(0),
                    },
                    Name::new("Some")
                ),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_same_module_function_call_data_constructor() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    typedef Identity[t] = Identity(t)
    let example = Identity(123)
            ",
        );

        let actual = maybe_find_example(&db, module_id).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::TypeDefinition(Fql {
                    module_id,
                    local_id: idx!(0),
                }),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
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

        let actual =
            resolve_expression_by_id(&db, module_id, idx!(1)).expect("must find expression");

        assert_eq!(
            Expression::FunctionCall {
                target: EPTdFql::Pattern(Fql {
                    module_id,
                    local_id: idx!(0),
                }),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
                }],
            },
            actual,
        );
    }

    #[test]
    fn resolve_same_module_function_call_trait_reference() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    trait TestTrait where
      self = #Type[_]
      typeof test_func : (a -> self[b]) -> self[a] -> self[b] where
        typevar a
        typevar b
    end

    typeof example : (t1 -> m[t2]) -> m[t1] -> m[t2] where
      typevar m = TestTrait
      typevar t1
      typevar t2
    let example = |transfomer, first| -> TestTrait::test_func(transformer, first)
",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), idx!(1))
            .unwrap_or_else(|| panic!("expected expression. hir_module: {:#?}", hir_module));

        let actual = resolve_expression_by_id(&db, module_id, idx).expect("must find expression");

        assert_eq!(
            Expression::Lambda {
                args: vec![
                    Fql {
                        module_id,
                        local_id: idx!(0),
                    },
                    Fql {
                        module_id,
                        local_id: idx!(1),
                    },
                ],
                body: Fql {
                    module_id,
                    local_id: idx!(2),
                }
            },
            actual,
        );

        // TODO: resolve function body
        // let body = resolve_expression_by_id(&db, module_id, idx!(2)).expect("must find expression");
        // assert_eq!(
        //     Expression::FunctionCall {
        //         target: EPTdFql::Expression(Fql {
        //             module_id,
        //             local_id: idx!(0),
        //         }),
        //         variant_name: None,
        //         args: vec![],
        //     },
        //     body,
        // );
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

        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownRootModule {
                attempted_module_path: ne_vec![hir::Name::new("unknown")],
            },
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(0),
            }),
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

        let expected = HirResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: idx!(0),
            },
            module_id: ModuleId::new(&db, "other"),
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

        let expected = HirResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: idx!(0),
            },
            module_id: ModuleId::new(&db, "other"),
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

        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownRootModule {
                attempted_module_path: ne_vec![hir::Name::new("not_other")],
            },
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(0),
            }),
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
                    local_id: idx!(0),
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

        let std_lib_root_module = VirtualModuleId::new(&db, "std");
        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownChildModule {
                module_id: std_lib_root_module,
                unknown_child: Name::new("unknown"),
                available_child_modules: db
                    .get_virtual_source(std_lib_root_module)
                    .children
                    .clone(),
            },
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(0),
            }),
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

        let expected = HirResolutionError::UnknownTypeDefinitionVariant {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(0),
            }),
            target_type_fql: Fql {
                module_id: ModuleId::new(&db, "std::option"),
                local_id: idx!(0),
            },
            variant_name: Name::new("Other"),
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
                    local_id: idx!(3),
                }),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
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
                target: EPTdFql::TypeDefinitionVariant(
                    Fql {
                        module_id: ModuleId::new(&db, "std::option"),
                        local_id: idx!(0),
                    },
                    Name::new("Some")
                ),
                args: vec![Fql {
                    module_id,
                    local_id: idx!(0),
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

        let expected = HirResolutionError::UnknownTypeDefinitionVariant {
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(1),
            }),
            target_type_fql: Fql {
                module_id: ModuleId::new(&db, "std::option"),
                local_id: idx!(0),
            },
            variant_name: Name::new("Other"),
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

        let expected = HirResolutionError::UnresolvedModule {
            err: hir::FqnResolutionError::UnknownRootModule {
                attempted_module_path: ne_vec![hir::Name::new("not_other")],
            },
            source_ref: EPTrFql::Expression(Fql {
                module_id,
                local_id: idx!(1),
            }),
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

        let expected = HirResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: idx!(0),
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

        let expected = HirResolutionError::UnknownExpressionReference {
            source_ref: Fql {
                module_id,
                local_id: idx!(1),
            },
            module_id,
            path: ne_vec![Name::new("unknown")],
        };

        assert_eq!(expected, err);
    }
}
