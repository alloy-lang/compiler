use super::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved::Fql;

pub(super) fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    then_branch: Fql<hir::Expression>,
    else_branch: Fql<hir::Expression>,
) -> MonoType {
    let cond_ty = super::infer_expr_hm(ctx, condition);
    ctx.add_equation(
        MonoType::Concrete(hir::BuiltInType::Bool),
        cond_ty,
        source_fql.clone(),
    );

    let then_ty = super::infer_expr_hm(ctx, then_branch);
    let else_ty = super::infer_expr_hm(ctx, else_branch);

    ctx.add_equation(then_ty.clone(), else_ty, source_fql.clone());

    ctx.assign_type(source_fql, then_ty)
}
