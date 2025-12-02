use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

use super::super::Fql;
use super::{expr_function_call, expr_variable_reference, HMInferenceContext, MonoType};

/// Generate constraints for an expression using HM inference
pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) -> MonoType {
    let fql = Fql::new(module_id, expr_id);
    let (hir_module, _) = hir::lower_file(ctx.db, fql.module_id);
    let expr = hir_module.get_expression(expr_id);

    match expr {
        hir::Expression::Literal(lit) => super::infer_literal(ctx, fql, lit),
        hir::Expression::Unit => super::infer_unit(ctx, fql),
        hir::Expression::VariableRef { path, scope } => {
            expr_variable_reference::infer_variable_ref(ctx, fql, path, *scope)
        }
        hir::Expression::Lambda { args, body } => infer_lambda(ctx, fql, args, *body),
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => expr_function_call::infer_function_call(ctx, fql, target, *scope, args),
        hir::Expression::Binary { lhs, rhs, .. } => infer_binary(ctx, fql, *lhs, *rhs),
        hir::Expression::Tuple(elements) => infer_tuple_expr(ctx, fql, elements),
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => infer_if_then_else(ctx, fql, *condition, *then, *else_),
        hir::Expression::Unary { expression, .. } => infer_unary(ctx, fql, *expression),
        hir::Expression::Match { condition, targets } => infer_match(ctx, fql, *condition, targets),
        hir::Expression::Missing => infer_missing_expr(ctx, fql),
    }
}

fn infer_lambda(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    args: &[hir::PatternIdx],
    body: hir::ExpressionIdx,
) -> MonoType {
    // Each lambda parameter gets a fresh type variable
    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_ty = super::pattern::infer_pattern_hm(ctx, fql.module_id, *arg_id);
        arg_types.push(arg_ty);
    }

    // Infer the body type
    let body_ty = infer_expr_hm(ctx, fql.module_id, body);

    // Build curried function type: arg1 -> (arg2 -> (... -> body))
    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(fql, func_ty)
}

fn infer_binary(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    lhs: hir::ExpressionIdx,
    rhs: hir::ExpressionIdx,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    // ie. for arithmetic operators, both sides should be numeric types
    // ie. for comparison operators, both sides should be comparable types, etc.
    let lhs_ty = infer_expr_hm(ctx, fql.module_id, lhs);
    let rhs_ty = infer_expr_hm(ctx, fql.module_id, rhs);

    // For now, assume both sides have the same type and return that type
    ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), fql.clone());

    let result_ty = lhs_ty;
    ctx.assign_type(fql, result_ty)
}

fn infer_tuple_expr(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    elements: &NonEmpty<hir::ExpressionIdx>,
) -> MonoType {
    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_ty = infer_expr_hm(ctx, fql.module_id, *elem_id);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(fql, ty)
}

fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    condition: hir::ExpressionIdx,
    then_branch: hir::ExpressionIdx,
    else_branch: hir::ExpressionIdx,
) -> MonoType {
    // Infer condition type and constrain it to Bool
    let cond_ty = infer_expr_hm(ctx, fql.module_id, condition);
    ctx.add_equation(
        cond_ty,
        MonoType::Concrete(hir::BuiltInType::Bool),
        fql.clone(),
    );

    let then_ty = infer_expr_hm(ctx, fql.module_id, then_branch);
    let else_ty = infer_expr_hm(ctx, fql.module_id, else_branch);

    // Both branches must have the same type
    ctx.add_equation(then_ty.clone(), else_ty, fql.clone());

    ctx.assign_type(fql, then_ty)
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    expression: hir::ExpressionIdx,
) -> MonoType {
    // For unary operations, infer the inner expression type
    let inner_ty = infer_expr_hm(ctx, fql.module_id, expression);

    // The result has the same type as the inner expression
    ctx.assign_type(fql, inner_ty)
}

fn infer_match(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    condition: hir::ExpressionIdx,
    targets: &[(hir::PatternIdx, hir::ExpressionIdx)],
) -> MonoType {
    // Infer the scrutinee type
    let value_ty = infer_expr_hm(ctx, fql.module_id, condition);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern_ty = super::pattern::infer_pattern_hm(ctx, fql.module_id, *pattern_id);
        ctx.add_equation(
            pattern_ty,
            value_ty.clone(),
            Fql::new(fql.module_id, *pattern_id),
        );

        // Body must have the same type as other arms
        let body_ty = infer_expr_hm(ctx, fql.module_id, *body_id);
        ctx.add_equation(
            body_ty,
            result_ty.clone(),
            Fql::new(fql.module_id, *body_id),
        );
    }

    ctx.assign_type(fql, result_ty)
}

pub(super) fn infer_missing_expr(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
) -> MonoType {
    // Missing expressions get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}
