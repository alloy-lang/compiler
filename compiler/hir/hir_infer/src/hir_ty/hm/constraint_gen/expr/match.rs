use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use alloy_hir_def as hir;
use alloy_hir_resolved::Fql;

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    targets: &[(Fql<hir::Pattern>, Fql<hir::Expression>)],
) -> MonoType {
    let value_ty = super::infer_expr_hm(ctx, condition);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern_ty = infer_pattern_hm(ctx, pattern_id.clone());
        ctx.add_equation(value_ty.clone(), pattern_ty, pattern_id.clone());

        // Body must have the same type as other arms
        let body_ty = super::infer_expr_hm(ctx, body_id.clone());
        ctx.add_equation(result_ty.clone(), body_ty, body_id.clone());
    }

    ctx.assign_type(source_fql, result_ty)
}
