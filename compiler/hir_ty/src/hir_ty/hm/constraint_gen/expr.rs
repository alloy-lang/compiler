use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{EPFql, Fql};
use non_empty_vec::NonEmpty;

/// Generate constraints for an expression using HM inference
pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    // Check if we've already inferred this expression
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    let expr =
        alloy_hir_resolved::resolve_expression(ctx.db, source_fql.module_id, source_fql.local_id);

    match expr {
        res::Expression::Literal(lit) => super::infer_literal(ctx, source_fql, lit),
        res::Expression::Unit => super::infer_unit(ctx, source_fql),
        res::Expression::VariableRef(ref_fql) => infer_variable_ref(ctx, source_fql, ref_fql),
        res::Expression::Lambda { args, body } => infer_lambda(ctx, source_fql, args, body),
        res::Expression::FunctionCall { target, args } => {
            infer_function_call(ctx, source_fql, target, args)
        }
        res::Expression::Binary { lhs, rhs, .. } => infer_binary(ctx, source_fql, lhs, rhs),
        res::Expression::Tuple(elements) => infer_tuple_expr(ctx, source_fql, elements),
        res::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => infer_if_then_else(ctx, source_fql, condition, then, else_),
        res::Expression::Unary { expression, .. } => infer_unary(ctx, source_fql, expression),
        res::Expression::Match { condition, targets } => {
            infer_match(ctx, source_fql, condition, &targets)
        }
        res::Expression::Missing => infer_missing_expr(ctx, source_fql),
        res::Expression::UnknownReference {
            source_ref,
            module_id: _,
            path: _,
        } => ctx.unknown_reference(source_ref),
    }
}

fn infer_variable_ref(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    ref_fql: EPFql,
) -> MonoType {
    let ty = match &ref_fql {
        EPFql::Expression(expr_fql) => infer_expr_hm(ctx, expr_fql.clone()),
        EPFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
    };
    ctx.assign_type(source_fql, ty.clone())
}

fn infer_lambda(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    args: Vec<Fql<hir::Pattern>>,
    body: Fql<hir::Expression>,
) -> MonoType {
    // Each lambda parameter gets a fresh type variable
    let arg_types = args
        .iter()
        .map(|arg_id| infer_pattern_hm(ctx, arg_id.clone()))
        .collect::<Vec<_>>();

    // Infer the body type
    let body_ty = infer_expr_hm(ctx, body);

    // Build curried function type: arg1 -> (arg2 -> (... -> body))
    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(source_fql, func_ty)
}

fn infer_function_call(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    target: EPFql,
    args: Vec<Fql<hir::Expression>>,
) -> MonoType {
    let func_ty = match target {
        EPFql::Expression(expr_fql) => infer_expr_hm(ctx, expr_fql),
        EPFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql),
    };

    let arg_types = args
        .iter()
        .map(|arg_fql| infer_expr_hm(ctx, arg_fql.clone()))
        .collect::<Vec<_>>();

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, fql.clone());

    ctx.assign_type(fql, result_ty)
}

fn infer_binary(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    lhs: Fql<hir::Expression>,
    rhs: Fql<hir::Expression>,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    // ie. for arithmetic operators, both sides should be numeric types
    // ie. for comparison operators, both sides should be comparable types, etc.
    let lhs_ty = infer_expr_hm(ctx, lhs);
    let rhs_ty = infer_expr_hm(ctx, rhs);

    // For now, assume both sides have the same type and return that type
    ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), source_fql.clone());

    let result_ty = lhs_ty;
    ctx.assign_type(source_fql, result_ty)
}

fn infer_tuple_expr(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    elements: NonEmpty<Fql<hir::Expression>>,
) -> MonoType {
    let element_types = elements
        .iter()
        .map(|elem_id| infer_expr_hm(ctx, elem_id.clone()))
        .collect();

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(source_fql, ty)
}

fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    then_branch: Fql<hir::Expression>,
    else_branch: Fql<hir::Expression>,
) -> MonoType {
    // Infer condition type and constrain it to Bool
    let cond_ty = infer_expr_hm(ctx, condition);
    ctx.add_equation(
        cond_ty,
        MonoType::Concrete(hir::BuiltInType::Bool),
        source_fql.clone(),
    );

    let then_ty = infer_expr_hm(ctx, then_branch);
    let else_ty = infer_expr_hm(ctx, else_branch);

    // Both branches must have the same type
    ctx.add_equation(then_ty.clone(), else_ty, source_fql.clone());

    ctx.assign_type(source_fql, then_ty)
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    inner: Fql<hir::Expression>,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    let inner_ty = infer_expr_hm(ctx, inner);
    ctx.assign_type(source_fql, inner_ty)
}

fn infer_match(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    targets: &[(Fql<hir::Pattern>, Fql<hir::Expression>)],
) -> MonoType {
    // Infer the scrutinee type
    let value_ty = infer_expr_hm(ctx, condition);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern_ty = infer_pattern_hm(ctx, pattern_id.clone());
        ctx.add_equation(pattern_ty, value_ty.clone(), pattern_id.clone());

        // Body must have the same type as other arms
        let body_ty = infer_expr_hm(ctx, body_id.clone());
        ctx.add_equation(body_ty, result_ty.clone(), body_id.clone());
    }

    ctx.assign_type(source_fql, result_ty)
}

pub(super) fn infer_missing_expr(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    // Missing expressions get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}
