use crate::hir_ty::hm::constraint_gen::resolve_cross_module_expression;
use crate::hir_ty::hm::HMInferenceContext;
use crate::hir_ty::{ExpressionOrPatternIdx, Fql, MonoType};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

pub(super) fn infer_variable_ref(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    path: &hir::Path,
    scope: ScopeIdx,
) -> MonoType {
    match path {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        } => infer_variable_ref_this_module(ctx, module_id, idx, scope, names),
        hir::Path::OtherModule(fqn) => infer_variable_ref_other_module(ctx, idx, fqn),
        hir::Path::Unknown(_) => super::expr::infer_missing_expr(ctx, idx),
    }
}

fn infer_variable_ref_this_module(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    scope: ScopeIdx,
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Try to find as an expression first
    if let Some((var_id, _)) = hir_module.get_expression_by_name(name, scope) {
        let var_fql = Fql::new(module_id, var_id);
        let var_idx = ExpressionOrPatternIdx::Expression(var_fql);

        // Check if we have a polymorphic type for this variable
        if let Some(poly_ty) = ctx.poly_env.get(&var_idx) {
            // Instantiate with fresh type variables
            let ty = poly_ty.instantiate(&mut ctx.type_var_gen);
            ctx.assign_type(idx, ty)
        } else if let Some(mono_ty) = ctx.type_env.get(&var_idx).cloned() {
            // Create a fresh type variable for this reference and add an equation
            // This allows bidirectional information flow
            let ref_ty = ctx.fresh_type_var();
            ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
            ctx.assign_type(idx, ref_ty)
        } else {
            // Variable not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty)
        }
    } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        // Try to find as a pattern (e.g., lambda parameter)
        let pat_fql = Fql::new(module_id, pat_id);
        let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

        if let Some(mono_ty) = ctx.type_env.get(&pat_idx).cloned() {
            // Create a fresh type variable for this reference and add an equation
            let ref_ty = ctx.fresh_type_var();
            ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
            ctx.assign_type(idx, ref_ty)
        } else {
            // Pattern not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty)
        }
    } else {
        // Variable not found in scope, create fresh type variable
        // This can happen for trait members or other unresolved references
        ctx.unknown_reference(idx)
    }
}

fn infer_variable_ref_other_module(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module reference
    if let Some((other_module_id, expr_id)) = resolve_cross_module_expression(ctx, fqn) {
        let var_fql = Fql::new(other_module_id, expr_id);
        let var_idx = ExpressionOrPatternIdx::Expression(var_fql);

        // Check if we have a polymorphic type for this variable
        if let Some(poly_ty) = ctx.poly_env.get(&var_idx) {
            // Instantiate with fresh type variables
            let ty = poly_ty.instantiate(&mut ctx.type_var_gen);
            return ctx.assign_type(idx, ty);
        } else if let Some(mono_ty) = ctx.type_env.get(&var_idx).cloned() {
            // Create a fresh type variable for this reference and add an equation
            let ref_ty = ctx.fresh_type_var();
            ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
            return ctx.assign_type(idx, ref_ty);
        }
    }
    // If resolution fails, use a fresh type variable
    ctx.unknown_reference(idx)
}
