use crate::hir_ty::hm::constraint_gen::infer_expr_hm;
use crate::hir_ty::hm::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved::Fql;
use non_empty_vec::NonEmpty;

pub(super) fn infer(
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
