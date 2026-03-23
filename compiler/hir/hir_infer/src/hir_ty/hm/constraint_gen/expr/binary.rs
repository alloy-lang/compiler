use super::{function_call, HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    lhs: Fql<hir::Expression>,
    rhs: Fql<hir::Expression>,
    op: hir::BinaryOp,
) -> MonoType {
    match op {
        hir::BinaryOp::Add | hir::BinaryOp::Sub | hir::BinaryOp::Mul | hir::BinaryOp::Div => {
            // Arithmetic operators: both sides should be numeric (for simplicity, we'll just use a type variable)
            let lhs_ty = super::infer_expr_hm(ctx, lhs.clone());
            let rhs_ty = super::infer_expr_hm(ctx, rhs.clone());

            let num_ty = ctx.fresh_type_var();
            ctx.add_equation(lhs_ty.clone(), num_ty.clone(), source_fql.clone());
            ctx.add_equation(rhs_ty.clone(), num_ty.clone(), source_fql.clone());
            return ctx.assign_type(source_fql, num_ty);
        }
        hir::BinaryOp::Custom(path) => {
            if let Ok(op_expr) = res::resolve_custom_binary_operator(ctx.db, &source_fql, &path) {
                return function_call::infer(ctx, source_fql.clone(), &op_expr.into(), &[lhs, rhs]);
            }
        }
        hir::BinaryOp::Missing => {}
    }
    let result_type = ctx.fresh_type_var();
    ctx.assign_type(source_fql, result_type)
}
