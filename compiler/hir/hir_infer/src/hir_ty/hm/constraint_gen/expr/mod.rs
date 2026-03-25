use super::super::TypeVarId;
use super::{HMInferenceContext, MonoType};
use crate::hir_ty::InferredType;
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;
use rustc_hash::FxHashMap;

mod binary;
mod function_call;
mod if_then_else;
mod lambda;
mod r#match;
mod r#trait;
mod tuple;
mod type_def;
mod value;
mod variable_ref;

pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    // Use `infer_value_signature` for cross-module value definitions and same-module definitions with polymorphic annotations
    // (when referenced from other definitions, not when being directly inferred).
    //
    // Skip the shortcut for:
    // - The expression currently being inferred (inferring_expr) to prevent cycles
    //   and ensure sub-expression types are collected
    // - Same-module unannotated definitions, so call-site constraints flow back
    let is_self = ctx
        .inferring_expr
        .as_ref()
        .is_some_and(|e| *e == source_fql);
    if !is_self {
        if let Some(value_def) =
            hir::module_value_def(ctx.db, source_fql.module_id, source_fql.local_id)
        {
            let is_cross_module = source_fql.module_id != ctx.module_id;
            let has_poly_annotation = value_def.type_annotation(ctx.db).is_some_and(|ta| {
                res::resolve_annotated_type(ctx.db, source_fql.module_id, ta).is_polymorphic()
            });

            if is_cross_module || has_poly_annotation {
                let sig = value::infer(ctx.db, value_def);
                let mono_ty = inferred_to_mono(&sig, ctx);
                return ctx.generalize_to_poly(mono_ty, &source_fql);
            }
        }
    }

    let expr =
        match res::resolve_expression_by_id(ctx.db, source_fql.module_id, source_fql.local_id) {
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

/// Convert an InferredType (from `infer_value_signature`) to a MonoType for use
/// in constraint generation. Generic IDs are mapped to fresh type variables,
/// with consistent mapping so the same Generic(id) produces the same TypeVarId.
fn inferred_to_mono(inferred: &InferredType, ctx: &mut HMInferenceContext) -> MonoType {
    let mut generic_map: FxHashMap<usize, TypeVarId> = FxHashMap::default();
    inferred_to_mono_inner(inferred, ctx, &mut generic_map)
}

fn inferred_to_mono_inner(
    inferred: &InferredType,
    ctx: &mut HMInferenceContext,
    generic_map: &mut FxHashMap<usize, TypeVarId>,
) -> MonoType {
    match inferred {
        InferredType::Unconstrained => MonoType::Unconstrained,
        InferredType::Missing => ctx.fresh_type_var(),
        InferredType::Unit => MonoType::Unit,
        InferredType::BuiltIn(b) => MonoType::Concrete(*b),
        InferredType::TypeDef(fql, name) => MonoType::TypeDef {
            fql: fql.clone(),
            type_args: vec![],
            type_def_name: name.clone(),
        },
        InferredType::Lambda {
            arg_type,
            return_type,
        } => MonoType::Function(
            Box::new(inferred_to_mono_inner(arg_type, ctx, generic_map)),
            Box::new(inferred_to_mono_inner(return_type, ctx, generic_map)),
        ),
        InferredType::Tuple(elements) => MonoType::Tuple(
            elements
                .iter()
                .map(|e| inferred_to_mono_inner(e, ctx, generic_map))
                .collect(),
        ),
        InferredType::Bounded { base, args } => {
            // When the base is a TypeDef, populate its type_args with fresh TypeVarIds
            // to preserve arity information (used in error messages and display)
            let constructor = match base.as_ref() {
                InferredType::TypeDef(fql, name) => {
                    let type_args = args.iter().map(|_| ctx.type_var_gen.fresh()).collect();
                    MonoType::TypeDef {
                        fql: fql.clone(),
                        type_args,
                        type_def_name: name.clone(),
                    }
                }
                other => inferred_to_mono_inner(other, ctx, generic_map),
            };
            MonoType::App {
                constructor: Box::new(constructor),
                args: args
                    .iter()
                    .map(|a| inferred_to_mono_inner(a, ctx, generic_map))
                    .collect(),
            }
        }
        InferredType::Generic(id) => {
            let var_id = *generic_map
                .entry(*id)
                .or_insert_with(|| ctx.type_var_gen.fresh());
            MonoType::Var(var_id)
        }
        InferredType::ConstrainedGeneric { id, .. } => {
            // TODO: Track constraints during solving
            let var_id = *generic_map
                .entry(*id)
                .or_insert_with(|| ctx.type_var_gen.fresh());
            MonoType::Var(var_id)
        }
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
