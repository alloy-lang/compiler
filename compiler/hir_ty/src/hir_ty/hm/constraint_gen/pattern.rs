use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

use super::super::{ExpressionOrPatternIdx, Fql};
use super::{resolve_cross_module_pattern, HMInferenceContext, MonoType};

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
        hir::Pattern::Literal(lit) => super::infer_literal(ctx, idx, lit),
        hir::Pattern::Unit => super::infer_unit(ctx, idx),
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
    ctx.assign_type(idx, ty)
}

fn infer_tuple_pattern(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    elements: &NonEmpty<hir::PatternIdx>,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_pattern = hir_module.get_pattern(*elem_id);
        let elem_ty = infer_pattern_hm(ctx, module_id, *elem_id, elem_pattern);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(idx, ty)
}

fn infer_pattern_ref(
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
        } => infer_pattern_ref_this_module(ctx, module_id, idx, scope, names),
        hir::Path::OtherModule(fqn) => infer_pattern_ref_other_module(ctx, idx, fqn),
        hir::Path::Unknown(names) => infer_missing_pattern(ctx, idx),
    }
}

fn infer_pattern_ref_this_module(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    idx: ExpressionOrPatternIdx,
    scope: ScopeIdx,
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, module_id);
    if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        let pat_fql = Fql::new(module_id, pat_id);
        let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

        // Check if we have a type for this pattern
        if let Some(mono_ty) = ctx.type_env.get(&pat_idx) {
            let ty = mono_ty.clone();
            ctx.assign_type(idx, ty)
        } else {
            // Pattern not found in environment, create fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty)
        }
    } else {
        // Pattern not found in scope, create fresh type variable
        ctx.unknown_reference(idx)
    }
}

fn infer_pattern_ref_other_module(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module pattern reference
    if let Some((other_module_id, pat_id)) = resolve_cross_module_pattern(ctx, fqn) {
        let pat_fql = Fql::new(other_module_id, pat_id);
        let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

        // Check if we have a type for this pattern
        if let Some(mono_ty) = ctx.type_env.get(&pat_idx).cloned() {
            return ctx.assign_type(idx, mono_ty);
        }
    }
    // If resolution fails, use a fresh type variable
    ctx.unknown_reference(idx)
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
    // TODO: In a full implementation, we'd look up the constructor's type scheme from target and scope
    let ty = ctx.fresh_type_var();
    ctx.assign_type(idx, ty)
}

fn infer_nil(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    // Nil pattern represents an empty list
    // In a full implementation, this would be `List[a]` where `a` is fresh
    // For now, just use a fresh type variable
    let ty = MonoType::Unconstrained;
    ctx.assign_type(idx, ty)
}

fn infer_missing_pattern(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    // Missing patterns get a fresh type variable
    let ty = MonoType::Missing;
    ctx.assign_type(idx, ty)
}
