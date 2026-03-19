use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use alloy_hir_def as hir;
use alloy_hir_resolved::{EPFql, Fql};

pub(super) fn infer(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    ref_fql: &EPFql,
) -> MonoType {
    // First check if this variable already has a type (possibly polymorphic)
    // This enables let-polymorphism: if the variable has been generalized,
    // we'll instantiate it with fresh type variables
    // Use tracked version: source_fql is the call site, ref_fql is the definition
    if let Some(existing_ty) = ctx.maybe_find_type(ref_fql.clone()) {
        return ctx.assign_type(source_fql, existing_ty);
    }

    // If not found, infer it (this handles forward references)
    let ty = match ref_fql {
        EPFql::Expression(expr_fql) => super::infer_expr_hm(ctx, expr_fql.clone()),
        EPFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
    };
    ctx.assign_type(source_fql, ty.clone())
}
