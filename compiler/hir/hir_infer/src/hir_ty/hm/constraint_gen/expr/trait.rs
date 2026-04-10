use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::infer_expr_hm;
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;

pub(super) fn infer_abstract_member_ref(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_annotation: Fql<hir::TypeReference>,
) -> MonoType {
    let annotated =
        res::resolve_annotated_type(ctx.db, type_annotation.module_id, type_annotation.local_id);

    let mono_ty = ctx.converter.annotated_to_mono(&annotated);
    ctx.assign_type(source_fql, mono_ty)
}

pub(super) fn infer_abstract_member_call(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_annotation: Fql<hir::TypeReference>,
    args: &[Fql<hir::Expression>],
) -> MonoType {
    let func_ty = infer_abstract_member_ref(ctx, source_fql.clone(), type_annotation);

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
    ctx.add_equation(func_ty, expected_func_ty, source_fql.clone());

    ctx.assign_type(source_fql, result_ty)
}
