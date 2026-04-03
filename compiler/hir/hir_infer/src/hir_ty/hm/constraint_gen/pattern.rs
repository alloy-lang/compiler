use super::{type_def, HMInferenceContext, MonoType};
use alloy_hir_def as hir;
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

    let pattern =
        match res::resolve_pattern_by_id(ctx.db, source_fql.module_id, source_fql.local_id) {
            Ok(p) => p,
            Err(err) => {
                return ctx.unknown_reference(err, source_fql);
            }
        };

    match pattern {
        res::Pattern::Literal(lit) => super::infer_literal(ctx, source_fql, &lit),
        res::Pattern::Unit => super::infer_unit(ctx, source_fql),
        res::Pattern::VariableDeclaration => infer_variable_declaration(ctx, source_fql),
        res::Pattern::Tuple(elements) => infer_tuple_pattern(ctx, source_fql, elements),
        res::Pattern::DataDestructure { target, args } => {
            infer_destructure(ctx, source_fql, &target, None, &args)
        }
        res::Pattern::VariantDestructure {
            target,
            variant_name,
            args,
        } => infer_destructure(ctx, source_fql, &target, Some(&variant_name), &args),
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
    source_fql: Fql<hir::Pattern>,
    target: &Fql<hir::TypeDefinition>,
    variant_name: Option<&hir::Name>,
    args: &[Fql<hir::Pattern>],
) -> MonoType {
    let field_pattern_types = args
        .iter()
        .map(|field_id| infer_pattern_hm(ctx, field_id.clone()))
        .collect::<Vec<_>>();

    let constructor_ty = match variant_name {
        Some(variant_name) => {
            type_def::infer_variant_constructor(ctx, &source_fql, target, variant_name.clone())
        }
        None => type_def::infer_data_constructor(ctx, &source_fql, target),
    };

    let (field_types, result_type) = decompose_function_type(constructor_ty);

    for (pattern_ty, field_ty) in field_pattern_types.iter().zip(field_types.iter()) {
        ctx.add_equation(pattern_ty.clone(), field_ty.clone(), source_fql.clone());
    }

    ctx.assign_type(source_fql, result_type)
}

/// Decompose a curried function type `a -> b -> ... -> result` into
/// `([a, b, ...], result)`. If the type is not a function, returns empty args.
fn decompose_function_type(ty: MonoType) -> (Vec<MonoType>, MonoType) {
    let mut args = Vec::new();
    let mut current = ty;
    while let MonoType::Function(arg, ret) = current {
        args.push(*arg);
        current = *ret;
    }
    (args, current)
}

fn infer_nil(ctx: &mut HMInferenceContext, fql: Fql<hir::Pattern>) -> MonoType {
    let ty = MonoType::Unconstrained;
    ctx.assign_type(fql, ty)
}

fn infer_missing_pattern(ctx: &mut HMInferenceContext, fql: Fql<hir::Pattern>) -> MonoType {
    // Missing patterns get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}
