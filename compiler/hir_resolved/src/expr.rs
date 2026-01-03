use super::{resolve_cross_module_expression, resolve_cross_module_type_definition, EPTdFql};
use crate::diagnostics::TypeResolutionError;
use crate::{EPFql, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
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
            resolve_function_call(db, source_ref, module_id, target, args)?
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
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    path: &hir::Path,
) -> Result<Expression, TypeResolutionError> {
    match path {
        hir::Path::ThisModule {
            name,
            subname,
            scope: this_scope,
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);

            // First try to resolve as a variable (expression or pattern)
            if let Some((var_id, _)) = hir_module.get_expression_by_name(name, *this_scope) {
                let var_fql = Fql::new(module_id, var_id);
                return Ok(Expression::VariableRef(var_fql.into()));
            }
            if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *this_scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                return Ok(Expression::VariableRef(pat_fql.into()));
            }

            // Check if this is an abstract trait member reference
            // (within a trait scope, referencing a member with a type annotation but no implementation)
            if let Some((trait_idx, trait_def)) =
                hir_module.find_trait_containing_scope(*this_scope)
            {
                // Check if this name is an abstract trait member
                for (member_name, type_annotation_idx) in trait_def.abstract_members() {
                    if member_name == name {
                        return Ok(Expression::AbstractTraitMemberRef {
                            trait_fql: Fql::new(module_id, trait_idx),
                            member_name: member_name.clone(),
                            type_annotation: Fql::new(module_id, type_annotation_idx),
                        });
                    }
                }
            }

            // Check if this is a qualified variant constructor (e.g., Option::None)
            if let Some(subname) = subname {
                let type_name = name;
                let variant_name = subname;

                if let Some((type_def_id, type_def)) =
                    hir_module.get_type_definition_by_name(type_name, *this_scope)
                {
                    return if type_def.kind.has_variant(variant_name) {
                        Ok(Expression::VariantConstructor {
                            type_def: Fql::new(module_id, type_def_id),
                            variant_name: variant_name.clone(),
                        })
                    } else {
                        return Err(TypeResolutionError::UnknownExpressionReference {
                            source_ref,
                            module_id,
                            path: ne_vec![type_name.clone(), variant_name.clone()],
                        });
                    };
                }
            }

            // lowering error
            Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: ne_vec![name.clone()],
            })
        }
        hir::Path::OtherModule(fqn) => {
            // Try to resolve as a regular expression reference
            match resolve_cross_module_expression(db, fqn, source_ref.clone()) {
                Ok(var_fql) => {
                    return Ok(Expression::VariableRef(var_fql.into()));
                }
                Err(err @ TypeResolutionError::UnknownModule { .. }) => {
                    // If the module doesn't exist, propagate that error immediately
                    return Err(err);
                }
                Err(_) => {
                    // Continue to try variant constructor
                }
            }

            // Try to resolve as a variant constructor
            if let Some(variant_name) = &fqn.sub_path {
                match resolve_cross_module_type_definition(db, fqn, source_ref.clone().into()) {
                    Ok(type_def_fql) => {
                        // Validate that the variant actually exists in the type definition
                        let (type_def_module, _) = hir::lower_file(db, type_def_fql.module_id);
                        let type_def = type_def_module.get_type_definition(type_def_fql.local_id);

                        if type_def.kind.has_variant(variant_name) {
                            return Ok(Expression::VariantConstructor {
                                type_def: type_def_fql,
                                variant_name: variant_name.clone(),
                            });
                        }
                        return Err(TypeResolutionError::UnknownExpressionReference {
                            source_ref,
                            module_id,
                            path: fqn.segments(),
                        });
                    }
                    Err(err @ TypeResolutionError::UnknownModule { .. }) => {
                        // If the module doesn't exist, propagate that error
                        return Err(err);
                    }
                    Err(_) => {
                        // Continue to final error
                    }
                }
            }

            Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: fqn.segments(),
            })
        }
        hir::Path::Unknown(names) => {
            // lowering error
            Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: names.clone(),
            })
        }
    }
}

fn resolve_function_call(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &[hir::ExpressionIdx],
) -> Result<Expression, TypeResolutionError> {
    let (fql, variant_name) = match target {
        hir::Path::ThisModule {
            name,
            subname,
            scope: this_scope,
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((expr_id, _)) = hir_module.get_expression_by_name(name, *this_scope) {
                let expr_fql = Fql::new(module_id, expr_id);
                (EPTdFql::Expression(expr_fql), None)
            } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *this_scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                (EPTdFql::Pattern(pat_fql), None)
            } else if let Some((td_id, _)) =
                hir_module.get_type_definition_by_name(name, *this_scope)
            {
                let td_fql = Fql::new(module_id, td_id);
                // Check if there's a variant subname (e.g., Option::Some)
                let variant = if let Some(subname) = subname {
                    // Verify that the subname is a valid variant
                    let type_def = hir_module.get_type_definition(td_id);
                    if type_def.kind.has_variant(subname) {
                        Some(subname.clone())
                    } else {
                        None
                    }
                } else {
                    None
                };
                (EPTdFql::TypeDefinition(td_fql), variant)
            } else {
                // lowering error
                return Err(TypeResolutionError::UnknownExpressionReference {
                    source_ref,
                    module_id,
                    path: ne_vec![name.clone()],
                });
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Ok(expr_fql) = resolve_cross_module_expression(db, fqn, source_ref.clone()) {
                (EPTdFql::Expression(expr_fql), None)
            } else if let Ok(td_fql) =
                resolve_cross_module_type_definition(db, fqn, source_ref.clone().into())
            {
                // TODO: Handle cross-module variant references (e.g., Other::Module::Option::Some)
                (EPTdFql::TypeDefinition(td_fql), None)
            } else {
                return Err(TypeResolutionError::UnknownExpressionReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                });
            }
        }
        hir::Path::Unknown(names) => {
            // lowering error
            return Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: names.clone(),
            });
        }
    };

    Ok(Expression::FunctionCall {
        target: fql,
        variant_name,
        args: args
            .iter()
            .map(|arg_id| Fql::new(module_id, *arg_id))
            .collect(),
    })
}

#[cfg(test)]
mod tests {
    use super::{resolve_expression_by_id, Expression};
    use crate::tests::TestHirResDatabase;
    use crate::{EPFql, EPTrFql, Fql, TypeResolutionError};
    use alloy_hir as hir;
    use alloy_hir::Name;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use la_arena::{Idx, RawIdx};
    use non_empty_vec::ne_vec;
    use std::fs;

    fn maybe_find_example(
        db: &dyn hir::HirDatabase,
        module_id: ModuleId,
    ) -> Result<Expression, TypeResolutionError> {
        let (hir_module, _) = hir::lower_file(db, module_id);
        let (idx, _expr) = hir_module
            .get_expression_by_name(&Name::new("example"), alloy_scope::Scopes::ROOT)
            .expect("expected expression");
        resolve_expression_by_id(db, module_id, idx)
    }

    fn find_example(db: &dyn hir::HirDatabase, module_id: ModuleId) -> Expression {
        let actual = maybe_find_example(db, module_id).expect("must find expression");
        let Expression::VariableRef(EPFql::Expression(fql)) = actual else {
            panic!("expected actual to be VariableRef, but was {:?}", actual);
        };

        resolve_expression_by_id(db, fql.module_id, fql.local_id).expect("must find expression")
    }

    fn build_db() -> TestHirResDatabase {
        let mut db = TestHirResDatabase::default();
        db.add_module(
            "std::option",
            camino::Utf8Path::new("/std/src/option.alloy"),
            fs::read_to_string("../../std/src/option.alloy")
                .expect("Expected to read std/src/option.alloy")
                .as_str(),
        );

        db
    }

    #[test]
    fn resolve_cross_module_expression_with_direct_import_unknown_variable() {
        let mut db = build_db();
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
    fn resolve_cross_module_expression_with_direct_import() {
        let mut db = build_db();
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
    fn resolve_cross_module_expression_with_extended_import() {
        let mut db = build_db();
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
    fn resolve_cross_module_expression_with_extra_path_returns_error() {
        let mut db = build_db();
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
    fn resolve_cross_module_expression_unknown_module() {
        let mut db = build_db();
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
    fn resolve_cross_module_type_def_with_incorrect_sub_path_returns_error() {
        let mut db = build_db();
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
}
