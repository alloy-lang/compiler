use crate::hir_ty::hm::constraint_gen::{infer_expr_hm, resolve_cross_module_expression};
use crate::hir_ty::hm::HMInferenceContext;
use crate::hir_ty::{ExpressionOrPatternIdx, Fql, MonoType};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

pub(super) fn infer_function_call(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    target: &hir::Path,
    scope: ScopeIdx,
    args: &[hir::ExpressionIdx],
) -> MonoType {
    match target {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        } => infer_function_call_this_module(ctx, module_id, idx, scope, args, names),
        hir::Path::OtherModule(fqn) => {
            infer_function_call_other_module(ctx, module_id, idx, args, fqn)
        }
        hir::Path::Unknown(_) => MonoType::Missing,
    }
}

fn infer_function_call_this_module(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    scope: ScopeIdx,
    args: &[hir::ExpressionIdx],
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let func_ty = if let Some((func_id, _)) = hir_module.get_expression_by_name(name, scope) {
        let func_expr = hir_module.get_expression(func_id);
        infer_expr_hm(ctx, module_id, func_id, func_expr)
    } else {
        // TODO: report unresolved function reference
        ctx.fresh_type_var()
    };

    // Infer argument types
    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_expr = hir_module.get_expression(*arg_id);
        let arg_ty = infer_expr_hm(ctx, module_id, *arg_id, arg_expr);
        arg_types.push(arg_ty);
    }

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, idx.clone());

    ctx.assign_type(idx, result_ty)
}

fn infer_function_call_other_module(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    args: &[hir::ExpressionIdx],
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module function reference
    let func_ty =
        if let Some((other_module_id, func_id)) = resolve_cross_module_expression(ctx, fqn) {
            let func_fql = Fql::new(other_module_id, func_id);
            let func_idx = ExpressionOrPatternIdx::Expression(func_fql);

            // Check if we have a polymorphic type for this function
            if let Some(poly_ty) = ctx.poly_env.get(&func_idx) {
                // Instantiate with fresh type variables
                poly_ty.instantiate(&mut ctx.type_var_gen)
            } else if let Some(mono_ty) = ctx.type_env.get(&func_idx).cloned() {
                mono_ty
            } else {
                ctx.fresh_type_var()
            }
        } else {
            // TODO: report unresolved function reference
            ctx.fresh_type_var()
        };

    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer argument types
    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_expr = hir_module.get_expression(*arg_id);
        let arg_ty = infer_expr_hm(ctx, module_id, *arg_id, arg_expr);
        arg_types.push(arg_ty);
    }

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, idx.clone());

    ctx.assign_type(idx, result_ty)
}
