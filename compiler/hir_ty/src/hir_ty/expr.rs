use crate::hir_ty::{
    pattern, type_reference, ExpressionOrPatternIdx, Fql, InferenceContext, ResolvedType,
    TypeRequirements,
};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;

pub fn collect_expr_type_fql(ctx: &mut InferenceContext, fql: &Fql<hir::Expression>) {
    collect_expr_type(ctx, fql.module_id, fql.local_id);
}

pub fn collect_expr_type(
    ctx: &mut InferenceContext,
    current_module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
) {
    let expression = {
        let (hir_module, _) = hir::lower_file(ctx.db, current_module_id);
        hir_module.get_expression(expression_id).clone()
    };

    println!("Collecting expression type: {expression:?}. id: {expression_id:?}");
    match &expression {
        hir::Expression::Missing => todo!("Missing expression"),
        hir::Expression::Literal(lit) => ctx.add_expr_requirements(
            current_module_id,
            expression_id,
            TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::from(lit))),
        ),
        hir::Expression::VariableRef { path, scope } => {
            collect_variable_ref(ctx, current_module_id, expression_id, path, scope);
        }
        hir::Expression::Binary { op: _, lhs, rhs } => {
            collect_binary_expr(ctx, current_module_id, expression_id, lhs, rhs);
        }
        hir::Expression::Unit => {
            ctx.insert_type(current_module_id, expression_id, ResolvedType::Unit);
        }
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => {
            ctx.insert_expr_type_variable(current_module_id, expression_id);

            ctx.insert_type(
                current_module_id,
                *condition,
                ResolvedType::BuiltIn(hir::BuiltInType::Bool),
            );
            collect_expr_type(ctx, current_module_id, *then);
            collect_expr_type(ctx, current_module_id, *else_);
        }
        hir::Expression::Tuple(inners) => {
            let inners = inners
                .iter()
                .map(|inner_id| Fql::new(current_module_id, *inner_id))
                .collect();

            let requirements = unsafe { TypeRequirements::Tuple(NonEmpty::new_unchecked(inners)) };
            ctx.add_expr_requirements(current_module_id, expression_id, requirements);
        }
        hir::Expression::Unary {
            op: _,
            expression: inner_id,
        } => {
            ctx.insert_expr_type_variable(current_module_id, expression_id);
            // todo: add "behavior impl" constraints based on the operator

            collect_expr_type(ctx, current_module_id, *inner_id);
        }
        hir::Expression::Lambda { args, body } => {
            for arg in args {
                pattern::collect_pattern_type(ctx, current_module_id, *arg);
            }
            collect_expr_type(ctx, current_module_id, *body);

            ctx.add_expr_requirements(
                current_module_id,
                expression_id,
                TypeRequirements::Lambda {
                    args: args
                        .iter()
                        .map(|arg_id| Fql::new(current_module_id, *arg_id))
                        .collect(),
                    body: Fql::new(current_module_id, *body),
                },
            );
        }
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => {
            collect_function_call(ctx, current_module_id, expression_id, target, scope, args);
        }
        hir::Expression::Match { .. } => {
            todo!("match")
        }
    }
}

fn collect_function_call(
    ctx: &mut InferenceContext,
    current_module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
    target: &hir::Path,
    scope: &ScopeIdx,
    call_args: &Vec<hir::ExpressionIdx>,
) {
    ctx.insert_expr_type_variable(current_module_id, expression_id);

    // Collect types for all arguments
    for arg in call_args {
        collect_expr_type(ctx, current_module_id, *arg);
    }

    if let Some(target_fql) = resolve_path(ctx.db, current_module_id, target, *scope) {
        collect_expr_type_fql(ctx, &target_fql);

        let func_expr = {
            let (hir_module, _) = hir::lower_file(ctx.db, target_fql.module_id);
            hir_module.get_expression(target_fql.local_id).clone()
        };

        // If the function is a lambda, create bidirectional constraints
        if let hir::Expression::Lambda {
            args: lambda_args,
            body,
        } = &func_expr
        {
            ctx.add_expr_requirements(
                target_fql.module_id,
                *body,
                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(Fql::new(
                    current_module_id,
                    expression_id,
                ))),
            );
            // Link each call argument to the corresponding lambda parameter (bidirectional)
            for (call_arg, lambda_param) in call_args.iter().zip(lambda_args.iter()) {
                let call_arg_fql = Fql::new(current_module_id, *call_arg);
                let lambda_param_fql = Fql::new(target_fql.module_id, *lambda_param);
                ctx.add_bidirectional_binding(
                    ExpressionOrPatternIdx::Expression(call_arg_fql),
                    ExpressionOrPatternIdx::Pattern(lambda_param_fql),
                );
            }
        }

        // Add a FunctionCall constraint that will be resolved during unification
        ctx.add_expr_requirements(
            current_module_id,
            expression_id,
            TypeRequirements::FunctionCall {
                func: ExpressionOrPatternIdx::Expression(target_fql),
                args: call_args
                    .iter()
                    .map(|arg_id| Fql::new(current_module_id, *arg_id))
                    .collect(),
            },
        );
    }
}

fn collect_variable_ref(
    ctx: &mut InferenceContext,
    current_module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
    path: &hir::Path,
    scope: &ScopeIdx,
) {
    ctx.insert_expr_type_variable(current_module_id, expression_id);

    match type_reference::resolve_path(ctx.db, current_module_id, path, *scope) {
        None => {
            println!("No type annotation for expression id: {expression_id:?}. path: {path:?}");
        }
        Some(fql_type_id) => {
            ctx.add_expr_requirements(
                current_module_id,
                expression_id,
                TypeRequirements::Annotated(fql_type_id),
            );
        }
    };

    let other_fql = pattern::resolve_path(ctx.db, current_module_id, path, *scope)
        .map(|pattern_fql| {
            pattern::collect_pattern_type_fql(ctx, &pattern_fql);
            ExpressionOrPatternIdx::Pattern(pattern_fql)
        })
        .or_else(|| {
            resolve_path(ctx.db, current_module_id, path, *scope).map(|expr_fql| {
                collect_expr_type_fql(ctx, &expr_fql);
                ExpressionOrPatternIdx::Expression(expr_fql)
            })
        });
    if let Some(other_fql) = other_fql {
        ctx.add_expr_requirements(
            current_module_id,
            expression_id,
            TypeRequirements::MustBeSameAs(other_fql),
        );
    }
}

fn collect_binary_expr(
    ctx: &mut InferenceContext,
    module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
    lhs: &hir::ExpressionIdx,
    rhs: &hir::ExpressionIdx,
) {
    ctx.insert_expr_type_variable(module_id, expression_id);

    collect_expr_type(ctx, module_id, *lhs);
    collect_expr_type(ctx, module_id, *rhs);

    // For now, assume binary operations preserve types (lhs, rhs, and result are all the same type)
    // TODO: Add proper "behavior impl" constraints based on the operator
    ctx.add_expr_requirements(
        module_id,
        expression_id,
        TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(Fql::new(
            module_id, *lhs,
        ))),
    );
    ctx.add_expr_requirements(
        module_id,
        expression_id,
        TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(Fql::new(
            module_id, *rhs,
        ))),
    );
}

pub fn resolve_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<Fql<hir::Expression>> {
    match path {
        hir::Path::ThisModule(this_path) => {
            get_expression_by_name(db, current_module_id, this_path.last(), scope)
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db
                .find_module_by_slug(&*module_slug)
                .expect("somehow, we couldn't find the module");
            get_expression_by_name(db, other_module_id, fqn.module.last(), Scopes::ROOT)
        }
        hir::Path::Unknown(_) => None,
    }
}

fn get_expression_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::Expression>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((idx, _)) = hir_module.get_expression_by_name(name, scope) else {
        return None;
    };

    Some(Fql::new(module_id, idx))
}
