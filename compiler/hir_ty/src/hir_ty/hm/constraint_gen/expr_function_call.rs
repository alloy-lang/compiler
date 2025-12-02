use crate::hir_ty::hm::constraint_gen::{infer_expr_hm, resolve_cross_module_expression};
use crate::hir_ty::hm::HMInferenceContext;
use crate::hir_ty::{Fql, MonoType};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use non_empty_vec::NonEmpty;

pub(super) fn infer_function_call(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    target: &hir::Path,
    scope: ScopeIdx,
    args: &[hir::ExpressionIdx],
) -> MonoType {
    match target {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        } => infer_function_call_this_module(ctx, fql, scope, args, names),
        hir::Path::OtherModule(fqn) => infer_function_call_other_module(ctx, fql, args, fqn),
        hir::Path::Unknown(_) => MonoType::Missing,
    }
}

fn infer_function_call_this_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    scope: ScopeIdx,
    args: &[hir::ExpressionIdx],
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, fql.module_id);

    let func_ty = if let Some((func_id, _)) = hir_module.get_expression_by_name(name, scope) {
        infer_expr_hm(ctx, fql.module_id, func_id)
    } else {
        ctx.unknown_reference(fql.clone())
    };

    // Infer argument types
    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_ty = infer_expr_hm(ctx, fql.module_id, *arg_id);
        arg_types.push(arg_ty);
    }

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, fql.clone());

    ctx.assign_type(fql, result_ty)
}

fn infer_function_call_other_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Expression>,
    args: &[hir::ExpressionIdx],
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module function reference
    let func_ty = if let Some(func_fql) = resolve_cross_module_expression(ctx, fqn) {
        ctx.find_type(func_fql)
    } else {
        ctx.unknown_reference(fql.clone())
    };

    // Infer argument types
    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_ty = infer_expr_hm(ctx, fql.module_id, *arg_id);
        arg_types.push(arg_ty);
    }

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, fql.clone());

    ctx.assign_type(fql, result_ty)
}
