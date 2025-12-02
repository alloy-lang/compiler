use alloy_hir as hir;
use alloy_workspace::ModuleId;

use super::super::{ExpressionOrPatternIdx, Fql};
use super::{expr_function_call, expr_variable_reference, HMInferenceContext, MonoType};

/// Generate constraints for an expression using HM inference
pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
    expr: &hir::Expression,
) -> MonoType {
    let fql = Fql::new(module_id, expr_id);
    let idx = ExpressionOrPatternIdx::Expression(fql);

    match expr {
        hir::Expression::Literal(lit) => super::infer_literal(ctx, idx, lit),
        hir::Expression::Unit => super::infer_unit(ctx, idx),
        hir::Expression::VariableRef { path, scope } => {
            expr_variable_reference::infer_variable_ref(ctx, module_id, idx, path, *scope)
        }
        hir::Expression::Lambda { args, body } => infer_lambda(ctx, module_id, idx, args, *body),
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => expr_function_call::infer_function_call(ctx, module_id, idx, target, *scope, args),
        hir::Expression::Binary { lhs, rhs, .. } => infer_binary(ctx, module_id, idx, *lhs, *rhs),
        hir::Expression::Tuple(elements) => infer_tuple_expr(ctx, module_id, idx, elements),
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => infer_if_then_else(ctx, module_id, idx, *condition, *then, *else_),
        hir::Expression::Unary { expression, .. } => infer_unary(ctx, module_id, idx, *expression),
        hir::Expression::Match { condition, targets } => {
            infer_match(ctx, module_id, idx, *condition, targets)
        }
        hir::Expression::Missing => infer_missing_expr(ctx, idx),
    }
}

fn infer_lambda(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    args: &[hir::PatternIdx],
    body: hir::ExpressionIdx,
) -> MonoType {
    // Each lambda parameter gets a fresh type variable
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_pattern = hir_module.get_pattern(*arg_id);
        let arg_ty = super::pattern::infer_pattern_hm(ctx, module_id, *arg_id, arg_pattern);
        arg_types.push(arg_ty);
    }

    // Infer the body type
    let body_expr = hir_module.get_expression(body);
    let body_ty = infer_expr_hm(ctx, module_id, body, body_expr);

    // Build curried function type: arg1 -> (arg2 -> (... -> body))
    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(idx, func_ty)
}

fn infer_binary(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    lhs: hir::ExpressionIdx,
    rhs: hir::ExpressionIdx,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    // ie. for arithmetic operators, both sides should be numeric types
    // ie. for comparison operators, both sides should be comparable types, etc.
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let lhs_expr = hir_module.get_expression(lhs);
    let lhs_ty = infer_expr_hm(ctx, module_id, lhs, lhs_expr);

    let rhs_expr = hir_module.get_expression(rhs);
    let rhs_ty = infer_expr_hm(ctx, module_id, rhs, rhs_expr);

    // For now, assume both sides have the same type and return that type
    ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), idx.clone());

    let result_ty = lhs_ty;
    ctx.assign_type(idx, result_ty)
}

fn infer_tuple_expr(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    elements: &non_empty_vec::NonEmpty<hir::ExpressionIdx>,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_expr = hir_module.get_expression(*elem_id);
        let elem_ty = infer_expr_hm(ctx, module_id, *elem_id, elem_expr);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(idx, ty)
}

fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    condition: hir::ExpressionIdx,
    then_branch: hir::ExpressionIdx,
    else_branch: hir::ExpressionIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer condition type and constrain it to Bool
    let cond_expr = hir_module.get_expression(condition);
    let cond_ty = infer_expr_hm(ctx, module_id, condition, cond_expr);
    ctx.add_equation(
        cond_ty,
        MonoType::Concrete(hir::BuiltInType::Bool),
        idx.clone(),
    );

    // Infer then branch
    let then_expr = hir_module.get_expression(then_branch);
    let then_ty = infer_expr_hm(ctx, module_id, then_branch, then_expr);

    // Infer else branch
    let else_expr = hir_module.get_expression(else_branch);
    let else_ty = infer_expr_hm(ctx, module_id, else_branch, else_expr);

    // Both branches must have the same type
    ctx.add_equation(then_ty.clone(), else_ty, idx.clone());

    ctx.assign_type(idx, then_ty)
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    expression: hir::ExpressionIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // For unary operations, infer the inner expression type
    let inner = hir_module.get_expression(expression);
    let inner_ty = infer_expr_hm(ctx, module_id, expression, inner);

    // The result has the same type as the inner expression
    ctx.assign_type(idx, inner_ty)
}

fn infer_match(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    condition: hir::ExpressionIdx,
    targets: &[(hir::PatternIdx, hir::ExpressionIdx)],
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer the scrutinee type
    let value_expr = hir_module.get_expression(condition);
    let value_ty = infer_expr_hm(ctx, module_id, condition, value_expr);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern = hir_module.get_pattern(*pattern_id);
        let pattern_ty = super::pattern::infer_pattern_hm(ctx, module_id, *pattern_id, pattern);
        ctx.add_equation(
            pattern_ty,
            value_ty.clone(),
            ExpressionOrPatternIdx::Pattern(Fql::new(module_id, *pattern_id)),
        );

        // Body must have the same type as other arms
        let body_expr = hir_module.get_expression(*body_id);
        let body_ty = infer_expr_hm(ctx, module_id, *body_id, body_expr);
        ctx.add_equation(
            body_ty,
            result_ty.clone(),
            ExpressionOrPatternIdx::Expression(Fql::new(module_id, *body_id)),
        );
    }

    ctx.assign_type(idx, result_ty)
}

pub(super) fn infer_missing_expr(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
) -> MonoType {
    // Missing expressions get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty)
}
