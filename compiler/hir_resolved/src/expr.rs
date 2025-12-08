use super::{resolve_cross_module_expression, resolve_cross_module_type_definition};
use crate::diagnostics::TypeResolutionError;
use crate::{EPFql, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::{ne_vec, NonEmpty};
use std::convert::TryFrom;

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
        target: EPFql,
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
            let scope = *this_scope;
            if let Some((trait_idx, trait_def)) = hir_module.find_trait_containing_scope(scope) {
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

            Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: ne_vec![name.clone()],
            })
        }
        hir::Path::OtherModule(fqn) => {
            // Try to resolve as a regular expression reference
            if let Ok(var_fql) = resolve_cross_module_expression(db, &fqn, source_ref.clone()) {
                return Ok(Expression::VariableRef(var_fql.into()));
            }

            // Try to resolve as a variant constructor
            if let Ok(sub_path) = NonEmpty::try_from(fqn.sub_path.clone()) {
                if let Ok(type_def_fql) =
                    resolve_cross_module_type_definition(db, &fqn, source_ref.clone().into())
                {
                    let variant_name = sub_path.last().clone();
                    return Ok(Expression::VariantConstructor {
                        type_def: type_def_fql,
                        variant_name,
                    });
                }
            }

            Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: fqn.segments(),
            })
        }
        hir::Path::Unknown(names) => Err(TypeResolutionError::UnknownExpressionReference {
            source_ref,
            module_id,
            path: names.clone(),
        }),
    }
}

fn resolve_function_call(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Expression>,
    module_id: ModuleId,
    target: &hir::Path,
    args: &Vec<hir::ExpressionIdx>,
) -> Result<Expression, TypeResolutionError> {
    let fql = match target {
        hir::Path::ThisModule {
            name,
            scope: this_scope,
            ..
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((expr_id, _)) = hir_module.get_expression_by_name(name, *this_scope) {
                let expr_fql = Fql::new(module_id, expr_id);
                EPFql::Expression(expr_fql)
            } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *this_scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                EPFql::Pattern(pat_fql)
            } else {
                return Err(TypeResolutionError::UnknownExpressionReference {
                    source_ref,
                    module_id,
                    path: ne_vec![name.clone()],
                });
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Ok(expr_fql) = resolve_cross_module_expression(db, &fqn, source_ref.clone()) {
                EPFql::Expression(expr_fql)
            } else {
                return Err(TypeResolutionError::UnknownExpressionReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                });
            }
        }
        hir::Path::Unknown(names) => {
            return Err(TypeResolutionError::UnknownExpressionReference {
                source_ref,
                module_id,
                path: names.clone(),
            })
        }
    };

    Ok(Expression::FunctionCall {
        target: fql,
        args: args
            .iter()
            .map(|arg_id| Fql::new(module_id, *arg_id))
            .collect(),
    })
}
