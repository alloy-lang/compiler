use crate::hir_ty::hm::constraint_gen::resolve_cross_module_expression;
use crate::hir_ty::hm::HMInferenceContext;
use crate::hir_ty::{Fql, MonoType};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use non_empty_vec::NonEmpty;

pub(super) fn infer_variable_ref(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    path: &hir::Path,
    scope: ScopeIdx,
) -> MonoType {
    match path {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        } => infer_variable_ref_this_module(ctx, fql, scope, names),
        hir::Path::OtherModule(fqn) => infer_variable_ref_other_module(ctx, fql, fqn),
        hir::Path::Unknown(_) => super::expr::infer_missing_expr(ctx, fql),
    }
}

fn infer_variable_ref_this_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    scope: ScopeIdx,
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, fql.module_id);

    // Try to find as an expression first
    let ty = if let Some((var_id, _)) = hir_module.get_expression_by_name(name, scope) {
        let var_fql = Fql::new(fql.module_id, var_id);
        ctx.find_type(var_fql)
    } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        let pat_fql = Fql::new(fql.module_id, pat_id);
        ctx.find_type(pat_fql)
    } else {
        // Variable not found in scope, create fresh type variable
        // This can happen for trait members or other unresolved references
        ctx.unknown_reference(fql.clone())
    };
    ctx.assign_type(fql, ty)
}

fn infer_variable_ref_other_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module reference
    let ty = if let Some(var_fql) = resolve_cross_module_expression(ctx, fqn) {
        ctx.find_type(var_fql)
    } else {
        // If resolution fails, use a fresh type variable
        ctx.unknown_reference(fql.clone())
    };
    ctx.assign_type(fql, ty)
}
