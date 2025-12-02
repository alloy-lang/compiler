use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

use super::super::Fql;
use super::{resolve_cross_module_pattern, HMInferenceContext, MonoType};

/// Generate constraints for a pattern using HM inference
pub(super) fn infer_pattern_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    pattern_id: hir::PatternIdx,
) -> MonoType {
    let fql = Fql::new(module_id, pattern_id);
    let (hir_module, _) = hir::lower_file(ctx.db, fql.module_id);
    let pattern = hir_module.get_pattern(pattern_id);

    match pattern {
        hir::Pattern::Literal(lit) => super::infer_literal(ctx, fql, lit),
        hir::Pattern::Unit => super::infer_unit(ctx, fql),
        hir::Pattern::VariableDeclaration { .. } => infer_variable_declaration(ctx, fql),
        hir::Pattern::Tuple(elements) => infer_tuple_pattern(ctx, fql, elements),
        hir::Pattern::PatternRef { path, scope } => infer_pattern_ref(ctx, fql, path, *scope),
        hir::Pattern::Destructure {
            target,
            scope,
            args,
        } => infer_destructure(ctx, fql, target, *scope, args),
        hir::Pattern::Nil => infer_nil(ctx, fql),
        hir::Pattern::Missing => infer_missing_pattern(ctx, fql),
    }
}

fn infer_variable_declaration(ctx: &mut HMInferenceContext, fql: Fql<hir::Pattern>) -> MonoType {
    // Fresh type variable for the bound variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}

fn infer_tuple_pattern(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    elements: &NonEmpty<hir::PatternIdx>,
) -> MonoType {
    let mut element_types = Vec::new();
    for elem_id in elements {
        let elem_ty = infer_pattern_hm(ctx, fql.module_id, *elem_id);
        element_types.push(elem_ty);
    }

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(fql, ty)
}

fn infer_pattern_ref(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    path: &hir::Path,
    scope: ScopeIdx,
) -> MonoType {
    match path {
        hir::Path::ThisModule {
            path: names,
            scope: _,
        } => infer_pattern_ref_this_module(ctx, fql, scope, names),
        hir::Path::OtherModule(fqn) => infer_pattern_ref_other_module(ctx, fql, fqn),
        hir::Path::Unknown(_) => infer_missing_pattern(ctx, fql),
    }
}

fn infer_pattern_ref_this_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    scope: ScopeIdx,
    names: &NonEmpty<hir::Name>,
) -> MonoType {
    let name = names.last();

    let (hir_module, _) = hir::lower_file(ctx.db, fql.module_id);
    let ty = if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, scope) {
        let pat_fql = Fql::new(fql.module_id, pat_id);
        ctx.find_type(pat_fql)
    } else {
        // Pattern not found in scope, create fresh type variable
        ctx.unknown_reference(fql.clone())
    };
    ctx.assign_type(fql, ty)
}

fn infer_pattern_ref_other_module(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    fqn: &hir::Fqn,
) -> MonoType {
    // Resolve cross-module pattern reference
    let ty = if let Some(pat_fql) = resolve_cross_module_pattern(ctx, fqn) {
        ctx.find_type(pat_fql)
    } else {
        // Pattern not found in scope, create fresh type variable
        ctx.unknown_reference(fql.clone())
    };
    ctx.assign_type(fql, ty)
}

fn infer_destructure(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    _target: &hir::Path,
    _scope: ScopeIdx,
    args: &[hir::PatternIdx],
) -> MonoType {
    // Infer types for all fields
    let mut _field_types = Vec::new();
    for field_id in args {
        let field_ty = infer_pattern_hm(ctx, fql.module_id, *field_id);
        _field_types.push(field_ty);
    }

    // For now, create a fresh type variable for the constructor application
    // TODO: In a full implementation, we'd look up the constructor's type scheme from target and scope
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}

fn infer_nil(ctx: &mut HMInferenceContext, fql: Fql<hir::Pattern>) -> MonoType {
    // Nil pattern represents an empty list
    // In a full implementation, this would be `List[a]` where `a` is fresh
    // For now, just use a fresh type variable
    let ty = MonoType::Unconstrained;
    ctx.assign_type(fql, ty)
}

fn infer_missing_pattern(ctx: &mut HMInferenceContext, fql: Fql<hir::Pattern>) -> MonoType {
    // Missing patterns get a fresh type variable
    let ty = MonoType::Missing;
    ctx.assign_type(fql, ty)
}
