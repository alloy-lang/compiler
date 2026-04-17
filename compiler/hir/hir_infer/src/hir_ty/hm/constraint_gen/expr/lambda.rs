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

    let return_var = ctx.fresh_type_var();
    let mut func_ty = return_var.clone();
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }
    let func_ty = ctx.assign_type(source_fql.clone(), func_ty);

    let body_ty = super::infer_expr_hm(ctx, body);
    ctx.add_equation(return_var, body_ty, source_fql);

    func_ty
}
