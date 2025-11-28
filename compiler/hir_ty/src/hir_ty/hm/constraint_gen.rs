//! Constraint generation for expressions and patterns
//!
//! This module walks through expressions and patterns, generating type equations
//! and assigning types to each node.

use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;

use super::super::{ExpressionOrPatternIdx, Fql};
use super::{HMInferenceContext, MonoType};

/// Generate constraints for an expression using HM inference
pub(super) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
    expr: &hir::Expression,
) -> MonoType {
    let fql = Fql::new(module_id, expr_id);
    let idx = ExpressionOrPatternIdx::Expression(fql);

    match expr {
        hir::Expression::Literal(lit) => infer_literal(ctx, idx, lit),
        hir::Expression::Unit => infer_unit(ctx, idx),
        hir::Expression::VariableRef { path, scope } => {
            infer_variable_ref(ctx, module_id, idx, path, *scope)
        }
        hir::Expression::Lambda { args, body } => infer_lambda(ctx, module_id, idx, args, *body),
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => infer_function_call(ctx, module_id, idx, target, *scope, args),
        hir::Expression::Binary { lhs, rhs, .. } => infer_binary(ctx, module_id, idx, *lhs, *rhs),
        hir::Expression::Tuple(elements) => infer_tuple_expr(ctx, module_id, idx, elements),
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => infer_if_then_else(ctx, module_id, idx, *condition, *then, *else_),
        hir::Expression::Unary { expression, .. } => infer_unary(ctx, module_id, idx, *expression),
        hir::Expression::Match { condition, targets } => {
            infer_match(ctx, module_id, idx, *condition, targets)
        }
        hir::Expression::Missing => infer_missing_expr(ctx, idx),
    }
}

fn infer_literal(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
    lit: &hir::Literal,
) -> MonoType {
    let ty = MonoType::Concrete(hir::BuiltInType::from(lit));
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_unit(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    let ty = MonoType::Unit;
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_variable_ref(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    path: &hir::Path,
    scope: ScopeIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Extract the last name from the path
    let name = match path {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        }
        | hir::Path::Unknown(names) => names.last(),
        hir::Path::OtherModule(_) => {
            // For now, use a fresh type variable for cross-module references
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            return ty;
        }
    };

    // Try to find as an expression first
    if let Some((var_id, _)) = hir_module.get_expression_by_name(name, scope) {
        let var_fql = Fql::new(module_id, var_id);
        let var_idx = ExpressionOrPatternIdx::Expression(var_fql);

        // Check if we have a polymorphic type for this variable
        if let Some(poly_ty) = ctx.poly_env.get(&var_idx) {
            // Instantiate with fresh type variables
            let ty = poly_ty.instantiate(&mut ctx.type_var_gen);
            ctx.assign_type(idx, ty.clone());
            ty
        } else if let Some(mono_ty) = ctx.type_env.get(&var_idx).cloned() {
            // Create a fresh type variable for this reference and add an equation
            // This allows bidirectional information flow
            let ref_ty = ctx.fresh_type_var();
            ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
            ctx.assign_type(idx, ref_ty.clone());
            ref_ty
        } else {
            // Variable not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }
    } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        // Try to find as a pattern (e.g., lambda parameter)
        let pat_fql = Fql::new(module_id, pat_id);
        let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

        if let Some(mono_ty) = ctx.type_env.get(&pat_idx).cloned() {
            // Create a fresh type variable for this reference and add an equation
            let ref_ty = ctx.fresh_type_var();
            ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
            ctx.assign_type(idx, ref_ty.clone());
            ref_ty
        } else {
            // Pattern not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }
    } else {
        // Variable not found in scope, create fresh type variable
        let ty = ctx.fresh_type_var();
        ctx.assign_type(idx, ty.clone());
        ty
    }
}

fn infer_lambda(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    args: &[hir::PatternIdx],
    body: hir::ExpressionIdx,
) -> MonoType {
    // Each lambda parameter gets a fresh type variable
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut arg_types = Vec::new();
    for arg_id in args {
        let arg_pattern = hir_module.get_pattern(*arg_id);
        let arg_ty = infer_pattern_hm(ctx, module_id, *arg_id, arg_pattern);
        arg_types.push(arg_ty);
    }

    // Infer the body type
    let body_expr = hir_module.get_expression(body);
    let body_ty = infer_expr_hm(ctx, module_id, body, body_expr);

    // Build curried function type: arg1 -> (arg2 -> (... -> body))
    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(idx, func_ty.clone());
    func_ty
}

fn infer_function_call(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    target: &hir::Path,
    scope: ScopeIdx,
    args: &[hir::ExpressionIdx],
) -> MonoType {
    // Infer the target function
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Extract the last name from the path
    let name = match target {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        }
        | hir::Path::Unknown(names) => names.last(),
        hir::Path::OtherModule(_) => {
            // For cross-module references, create a fresh function type
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            let mut arg_types = Vec::new();
            for arg_id in args {
                let arg_expr = hir_module.get_expression(*arg_id);
                let arg_ty = infer_expr_hm(ctx, module_id, *arg_id, arg_expr);
                arg_types.push(arg_ty);
            }

            let result_ty = ctx.fresh_type_var();
            ctx.assign_type(idx, result_ty.clone());
            return result_ty;
        }
    };

    let func_ty = if let Some((func_id, _)) = hir_module.get_expression_by_name(name, scope) {
        let func_expr = hir_module.get_expression(func_id);
        infer_expr_hm(ctx, module_id, func_id, func_expr)
    } else {
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

    ctx.assign_type(idx, result_ty.clone());
    result_ty
}

fn infer_binary(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    lhs: hir::ExpressionIdx,
    rhs: hir::ExpressionIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let lhs_expr = hir_module.get_expression(lhs);
    let lhs_ty = infer_expr_hm(ctx, module_id, lhs, lhs_expr);

    let rhs_expr = hir_module.get_expression(rhs);
    let rhs_ty = infer_expr_hm(ctx, module_id, rhs, rhs_expr);

    // For now, assume both sides have the same type and return that type
    ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), idx.clone());

    let result_ty = lhs_ty;
    ctx.assign_type(idx, result_ty.clone());
    result_ty
}

fn infer_tuple_expr(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    elements: &non_empty_vec::NonEmpty<hir::ExpressionIdx>,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_expr = hir_module.get_expression(*elem_id);
        let elem_ty = infer_expr_hm(ctx, module_id, *elem_id, elem_expr);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    condition: hir::ExpressionIdx,
    then_branch: hir::ExpressionIdx,
    else_branch: hir::ExpressionIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer condition type and constrain it to Bool
    let cond_expr = hir_module.get_expression(condition);
    let cond_ty = infer_expr_hm(ctx, module_id, condition, cond_expr);
    ctx.add_equation(
        cond_ty,
        MonoType::Concrete(hir::BuiltInType::Bool),
        idx.clone(),
    );

    // Infer then branch
    let then_expr = hir_module.get_expression(then_branch);
    let then_ty = infer_expr_hm(ctx, module_id, then_branch, then_expr);

    // Infer else branch
    let else_expr = hir_module.get_expression(else_branch);
    let else_ty = infer_expr_hm(ctx, module_id, else_branch, else_expr);

    // Both branches must have the same type
    ctx.add_equation(then_ty.clone(), else_ty, idx.clone());

    ctx.assign_type(idx, then_ty.clone());
    then_ty
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    expression: hir::ExpressionIdx,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // For unary operations, infer the inner expression type
    let inner = hir_module.get_expression(expression);
    let inner_ty = infer_expr_hm(ctx, module_id, expression, inner);

    // The result has the same type as the inner expression
    ctx.assign_type(idx, inner_ty.clone());
    inner_ty
}

fn infer_match(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    condition: hir::ExpressionIdx,
    targets: &[(hir::PatternIdx, hir::ExpressionIdx)],
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer the scrutinee type
    let value_expr = hir_module.get_expression(condition);
    let value_ty = infer_expr_hm(ctx, module_id, condition, value_expr);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern = hir_module.get_pattern(*pattern_id);
        let pattern_ty = infer_pattern_hm(ctx, module_id, *pattern_id, pattern);
        ctx.add_equation(pattern_ty, value_ty.clone(), idx.clone());

        // Body must have the same type as other arms
        let body_expr = hir_module.get_expression(*body_id);
        let body_ty = infer_expr_hm(ctx, module_id, *body_id, body_expr);
        ctx.add_equation(body_ty, result_ty.clone(), idx.clone());
    }

    ctx.assign_type(idx, result_ty.clone());
    result_ty
}

fn infer_missing_expr(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    // Missing expressions get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty.clone());
    ty
}

/// Generate constraints for a pattern using HM inference
pub(super) fn infer_pattern_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    pattern_id: hir::PatternIdx,
    pattern: &hir::Pattern,
) -> MonoType {
    let fql = Fql::new(module_id, pattern_id);
    let idx = ExpressionOrPatternIdx::Pattern(fql);

    match pattern {
        hir::Pattern::Literal(lit) => infer_literal(ctx, idx, lit),
        hir::Pattern::Unit => infer_unit(ctx, idx),
        hir::Pattern::VariableDeclaration { .. } => infer_variable_declaration(ctx, idx),
        hir::Pattern::Tuple(elements) => infer_tuple_pattern(ctx, module_id, idx, elements),
        hir::Pattern::PatternRef { path, scope } => {
            infer_pattern_ref(ctx, module_id, idx, path, *scope)
        }
        hir::Pattern::Destructure {
            target,
            scope,
            args,
        } => infer_destructure(ctx, module_id, idx, target, *scope, args),
        hir::Pattern::Nil => infer_nil(ctx, idx),
        hir::Pattern::Missing => infer_missing_pattern(ctx, idx),
    }
}

fn infer_variable_declaration(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
) -> MonoType {
    // Fresh type variable for the bound variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_tuple_pattern(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    elements: &non_empty_vec::NonEmpty<hir::PatternIdx>,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_pattern = hir_module.get_pattern(*elem_id);
        let elem_ty = infer_pattern_hm(ctx, module_id, *elem_id, elem_pattern);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_pattern_ref(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    path: &hir::Path,
    scope: ScopeIdx,
) -> MonoType {
    // Look up the pattern in the environment
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Extract the last name from the path
    let name = match path {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        }
        | hir::Path::Unknown(names) => names.last(),
        hir::Path::OtherModule(_) => {
            // For now, use a fresh type variable for cross-module references
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            return ty;
        }
    };

    if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        let pat_fql = Fql::new(module_id, pat_id);
        let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

        // Check if we have a type for this pattern
        if let Some(mono_ty) = ctx.type_env.get(&pat_idx) {
            let ty = mono_ty.clone();
            ctx.assign_type(idx, ty.clone());
            ty
        } else {
            // Pattern not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }
    } else {
        // Pattern not found in scope, create fresh type variable
        let ty = ctx.fresh_type_var();
        ctx.assign_type(idx, ty.clone());
        ty
    }
}

fn infer_destructure(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    _target: &hir::Path,
    _scope: ScopeIdx,
    args: &[hir::PatternIdx],
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    // Infer types for all fields
    let mut _field_types = Vec::new();
    for field_id in args {
        let field_pattern = hir_module.get_pattern(*field_id);
        let field_ty = infer_pattern_hm(ctx, module_id, *field_id, field_pattern);
        _field_types.push(field_ty);
    }

    // For now, create a fresh type variable for the constructor application
    // In a full implementation, we'd look up the constructor's type scheme from target and scope
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_nil(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    // Nil pattern represents an empty list
    // In a full implementation, this would be List[a] where a is fresh
    // For now, just use a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty.clone());
    ty
}

fn infer_missing_pattern(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    // Missing patterns get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty.clone());
    ty
}
