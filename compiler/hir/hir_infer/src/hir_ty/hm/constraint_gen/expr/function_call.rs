use super::super::pattern::infer_pattern_hm;
use super::type_def;
use super::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved::{EPTdFql, Fql};

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    target: &EPTdFql,
    args: &[Fql<hir::Expression>],
) -> MonoType {
    let func_ty = find_function_type(ctx, &source_fql, &target);

    let arg_types = args
        .iter()
        .map(|arg_fql| super::infer_expr_hm(ctx, arg_fql.clone()))
        .collect::<Vec<_>>();

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, source_fql.clone());

    ctx.assign_type(source_fql, result_ty)
}

fn find_function_type(
    ctx: &mut HMInferenceContext,
    source_fql: &Fql<hir::Expression>,
    target: &EPTdFql,
) -> MonoType {
    if let Some(tracked_ty) = ctx.maybe_find_type(target.clone()) {
        return tracked_ty;
    }

    match &target {
        EPTdFql::Expression(expr_fql) => super::infer_expr_hm(ctx, expr_fql.clone()),
        EPTdFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
        EPTdFql::TypeDefinition(td_fql) => {
            type_def::infer_data_constructor(ctx, source_fql.clone(), td_fql)
        }
        EPTdFql::TypeDefinitionVariant(td_fql, variant_name) => {
            type_def::infer_variant_constructor(
                ctx,
                source_fql.clone(),
                td_fql,
                variant_name.clone(),
            )
        }
    }
}
