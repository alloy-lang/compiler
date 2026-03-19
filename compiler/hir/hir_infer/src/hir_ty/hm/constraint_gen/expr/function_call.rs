use super::super::pattern::infer_pattern_hm;
use super::{type_def, HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved::{EPTdFql, Fql};

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    target: &EPTdFql,
    args: &[Fql<hir::Expression>],
) -> MonoType {
    // Try to get the function type with tracking if it's polymorphic
    // This handles cases like `id("hi")` where the target directly references a polymorphic function
    let func_ty = if let Some(tracked_ty) = ctx.maybe_find_type(target.clone()) {
        tracked_ty
    } else {
        // Not found or not polymorphic yet, infer it
        match &target {
            EPTdFql::Expression(expr_fql) => super::infer_expr_hm(ctx, expr_fql.clone()),
            EPTdFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
            EPTdFql::TypeDefinition(td_fql) => {
                // Calling the type definition itself (single-variant types)
                // Infer the type definition (e.g., variant constructors)
                // This will populate poly_env if it's polymorphic
                // TODO: handle tracking if it's polymorphic
                type_def::infer_type_definition(ctx, td_fql.clone());

                ctx.maybe_find_type(target.clone())
                    .unwrap_or_else(|| ctx.fresh_type_var())
            }
            EPTdFql::TypeDefinitionVariant(td_fql, variant_name) => {
                // Infer the specific variant constructor
                // This handles tracking if it's polymorphic and returns the instantiated type
                type_def::infer_variant_constructor(
                    ctx,
                    source_fql.clone(),
                    td_fql.clone(),
                    variant_name.clone(),
                )
            }
        }
    };

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
