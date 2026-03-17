use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use alloy_hir_def as hir;
use alloy_hir_resolved::Fql;

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    args: &[Fql<hir::Pattern>],
    body: Fql<hir::Expression>,
) -> MonoType {
    let arg_types = args
        .iter()
        .map(|arg_id| infer_pattern_hm(ctx, arg_id.clone()))
        .collect::<Vec<_>>();

    let body_ty = super::infer_expr_hm(ctx, body);

    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(source_fql, func_ty)
}
