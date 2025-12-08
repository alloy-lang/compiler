use super::{HMInferenceContext, MonoType};
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;
use non_empty_vec::NonEmpty;

/// Generate constraints for a pattern using HM inference
pub(super) fn infer_pattern_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Pattern>,
) -> MonoType {
    // Check if we've already inferred this pattern
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    let pattern = match alloy_hir_resolved::resolve_pattern_by_id(
        ctx.db,
        source_fql.module_id,
        source_fql.local_id,
    ) {
        Ok(p) => p,
        Err(err) => {
            ctx.report_resolution_error(err);
            return ctx.unknown_reference(source_fql);
        }
    };

    match pattern {
        res::Pattern::Literal(lit) => super::infer_literal(ctx, source_fql, lit.clone()),
        res::Pattern::Unit => super::infer_unit(ctx, source_fql),
        res::Pattern::VariableDeclaration => infer_variable_declaration(ctx, source_fql),
        res::Pattern::Tuple(elements) => infer_tuple_pattern(ctx, source_fql, elements),
        res::Pattern::Destructure { target, args } => {
            infer_destructure(ctx, source_fql, target, &args)
        }
        res::Pattern::Nil => infer_nil(ctx, source_fql),
        res::Pattern::Missing => infer_missing_pattern(ctx, source_fql),
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
    elements: NonEmpty<Fql<hir::Pattern>>,
) -> MonoType {
    let element_types = elements
        .iter()
        .map(|elem_id| infer_pattern_hm(ctx, elem_id.clone()))
        .collect();

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(fql, ty)
}

fn infer_destructure(
    ctx: &mut HMInferenceContext,
    fql: Fql<hir::Pattern>,
    _target: Fql<hir::TypeDefinition>,
    args: &[Fql<hir::Pattern>],
) -> MonoType {
    // Infer types for all fields
    let _field_types = args
        .iter()
        .map(|field_id| infer_pattern_hm(ctx, field_id.clone()))
        .collect::<Vec<_>>();

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
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}
