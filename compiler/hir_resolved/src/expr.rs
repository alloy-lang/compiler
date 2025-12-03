use super::resolve_cross_module_expression;
use crate::{EPFql, Fql};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Missing,
    UnknownReference {
        source_ref: Fql<hir::Expression>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
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
        target: EPFql,
        args: Vec<Fql<hir::Expression>>,
    },
    Match {
        condition: Fql<hir::Expression>,
        targets: Vec<(Fql<hir::Pattern>, Fql<hir::Expression>)>,
    },
}

pub fn resolve_expression(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) -> Expression {
    let source_ref = Fql::new(module_id, expr_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expr = hir_module.get_expression(expr_id);

    match expr {
        hir::Expression::Literal(lit) => Expression::Literal(lit.clone()),
        hir::Expression::Unit => Expression::Unit,
        hir::Expression::VariableRef { path, scope } => {
            resolve_variable_ref(db, source_ref, module_id, path, *scope)
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
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => resolve_function_call(db, source_ref, module_id, target, scope, args),
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
    }
}

fn resolve_variable_ref(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Expression {
    match path {
        hir::Path::ThisModule {
            path: names,
            scope: _, // check to see if the scope is the same as the expression's scope
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((var_id, _)) = hir_module.get_expression_by_name(names.last(), scope) {
                let var_fql = Fql::new(module_id, var_id);
                Expression::VariableRef(var_fql.into())
            } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(names.last(), scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                Expression::VariableRef(pat_fql.into())
            } else {
                Expression::UnknownReference {
                    source_ref,
                    module_id,
                    path: names.clone(),
                }
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(var_fql) = resolve_cross_module_expression(db, &fqn) {
                Expression::VariableRef(var_fql.into())
            } else {
                // If resolution fails, use a fresh type variable
                Expression::UnknownReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                }
            }
        }
        hir::Path::Unknown(names) => Expression::UnknownReference {
            source_ref,
            module_id,
            path: names.clone(),
        },
    }
}

fn resolve_function_call(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
    scope: &ScopeIdx,
    args: &Vec<hir::ExpressionIdx>,
) -> Expression {
    let fql = match target {
        hir::Path::ThisModule {
            path: names,
            scope: _, // check to see if the scope is the same as the expression's scope
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((expr_id, _)) = hir_module.get_expression_by_name(names.last(), *scope) {
                let expr_fql = Fql::new(module_id, expr_id);
                EPFql::Expression(expr_fql)
            } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(names.last(), *scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                EPFql::Pattern(pat_fql)
            } else {
                return Expression::UnknownReference {
                    source_ref,
                    module_id,
                    path: names.clone(),
                };
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(expr_fql) = resolve_cross_module_expression(db, &fqn) {
                EPFql::Expression(expr_fql)
            } else {
                return Expression::UnknownReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                };
            }
        }
        hir::Path::Unknown(names) => {
            return Expression::UnknownReference {
                source_ref,
                module_id,
                path: names.clone(),
            }
        }
    };

    Expression::FunctionCall {
        target: fql,
        args: args
            .iter()
            .map(|arg_id| Fql::new(module_id, *arg_id))
            .collect(),
    }
}
