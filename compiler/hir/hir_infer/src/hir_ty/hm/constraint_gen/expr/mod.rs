use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::inference::annotated_to_mono;
use crate::hir_ty::hm::PolyType;
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, Fql};

mod binary;
mod function_call;
mod if_then_else;
mod lambda;
mod r#match;
mod r#trait;
mod tuple;
mod type_def;
mod variable_ref;

pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    // Lazy constraint generation: if this expression is in a later group,
    // don't infer it now - just return a fresh type variable.
    // Only applies to expressions in the current module — cross-module expressions
    // have their own independent ordering and must always be inferred immediately.
    // DON'T assign to type_env to avoid polluting env_type_vars for generalization
    if source_fql.module_id == ctx.module_id && ctx.is_in_later_group(source_fql.local_id) {
        return ctx.fresh_type_var();
    }

    // For cross-module expressions with polymorphic type annotations, use the annotation
    // directly rather than re-inferring from the body. This ensures each use site gets
    // fresh type variables via poly_env instantiation, avoiding stale specialization
    // when a polymorphic cross-module function (e.g., <|) is used multiple times.
    if source_fql.module_id != ctx.module_id {
        let (other_hir_module, _) = hir::lower_file(ctx.db, source_fql.module_id);
        if let Some(value) = other_hir_module.get_value_by_id(&source_fql.local_id) {
            if let Some(type_annotation) = value.type_annotation {
                let annotated =
                    resolve_annotated_type(ctx.db, source_fql.module_id, type_annotation);
                if annotated.is_polymorphic() {
                    if let Some(mono_ty) = annotated_to_mono(&annotated, ctx) {
                        let poly_ty = PolyType::generalize_all(mono_ty);
                        ctx.poly_env.insert(source_fql.clone().into(), poly_ty);
                        // Return a fresh instantiation
                        if let Some(instantiated) = ctx.maybe_find_type(&source_fql) {
                            return instantiated;
                        }
                    }
                }
            }
        }
    }

    let expr = match alloy_hir_resolved::resolve_expression_by_id(
        ctx.db,
        source_fql.module_id,
        source_fql.local_id,
    ) {
        Ok(expr) => expr,
        Err(err) => {
            // Report the resolution error
            return ctx.unknown_reference(err, source_fql);
        }
    };

    match expr {
        res::Expression::Literal(lit) => super::infer_literal(ctx, source_fql, &lit),
        res::Expression::Unit => super::infer_unit(ctx, source_fql),
        res::Expression::VariableRef(ref_fql) => variable_ref::infer(ctx, source_fql, &ref_fql),
        res::Expression::Lambda { args, body } => lambda::infer(ctx, source_fql, &args, body),
        res::Expression::FunctionCall { target, args } => {
            function_call::infer(ctx, source_fql, &target, &args)
        }
        res::Expression::AbstractTraitFunctionCall {
            type_annotation,
            args,
            ..
        } => r#trait::infer_abstract_member_call(ctx, source_fql, type_annotation, &args),
        res::Expression::Binary { lhs, rhs, op } => binary::infer(ctx, source_fql, lhs, rhs, op),
        res::Expression::Tuple(elements) => tuple::infer(ctx, source_fql, elements),
        res::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => if_then_else::infer_if_then_else(ctx, source_fql, condition, then, else_),
        res::Expression::Unary { expression, .. } => infer_unary(ctx, source_fql, expression),
        res::Expression::Match { condition, targets } => {
            r#match::infer(ctx, source_fql, condition, &targets)
        }
        res::Expression::DataConstructor { type_def } => {
            type_def::infer_data_constructor(ctx, source_fql, &type_def)
        }
        res::Expression::VariantConstructor {
            type_def,
            variant_name,
        } => type_def::infer_variant_constructor(ctx, source_fql, type_def, variant_name),
        res::Expression::AbstractTraitMemberRef {
            trait_fql: _,
            member_name: _,
            type_annotation,
        } => r#trait::infer_abstract_member_ref(ctx, source_fql, type_annotation),
        res::Expression::Missing => infer_missing_expr(ctx, source_fql),
    }
}

fn infer_missing_expr(ctx: &mut HMInferenceContext, source_fql: Fql<hir::Expression>) -> MonoType {
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    inner: Fql<hir::Expression>,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    let inner_ty = infer_expr_hm(ctx, inner);
    ctx.assign_type(source_fql, inner_ty)
}
