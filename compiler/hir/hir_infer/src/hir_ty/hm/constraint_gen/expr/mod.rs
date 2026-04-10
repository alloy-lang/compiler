use super::{type_def, HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;

mod binary;
mod function_call;
mod if_then_else;
mod lambda;
mod r#match;
mod r#trait;
mod tuple;
mod value;
mod variable_ref;

pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    let Fql {
        module_id,
        local_id,
    } = &source_fql;

    if !ctx.matches_root(&source_fql) {
        if let Some(value_def) = hir::module_value_def(ctx.db, *module_id, *local_id) {
            let sig = value::infer(ctx.db, value_def);
            return ctx.converter.inferred_to_mono(&sig);
        }
    }

    let expr = match res::resolve_expression_by_id(ctx.db, *module_id, *local_id) {
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
        } => type_def::infer_variant_constructor(ctx, source_fql, &type_def, variant_name),
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
