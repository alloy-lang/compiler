//! Constraint generation for expressions and patterns
//!
//! This module walks through expressions and patterns, generating type equations
//! and assigning types to each node.
//!
//! # Outstanding TODOs:
//!
//! 1. Resolve trait functions that have been implemented for type definitions. For example,
//!    when both `typedef std::option::Option` and `trait std::monad::Monad` are imported,
//!    we should be able to resolve `Option::flat_map` to the trait implementation.
//!    This requires tracking which traits are implemented for which types and providing
//!    those methods in the namespace.

mod expr;
mod pattern;

use alloy_hir_def as hir;
use alloy_hir_resolved as res;

use super::{annotated_to_mono, HMInferenceContext, MonoType};

use alloy_hir_resolved::{resolve_annotated_type, EPTdFql, Fql};
pub(crate) use expr::infer_expr_hm;

fn infer_literal(
    ctx: &mut HMInferenceContext,
    fql: impl Into<EPTdFql>,
    lit: &hir::Literal,
) -> MonoType {
    let ty = MonoType::Concrete(hir::BuiltInType::from(lit));
    ctx.assign_type(fql, ty)
}

fn infer_unit(ctx: &mut HMInferenceContext, fql: impl Into<EPTdFql>) -> MonoType {
    let ty = MonoType::Unit;
    ctx.assign_type(fql, ty)
}

fn build_constructor_type(
    ctx: &mut HMInferenceContext,
    type_def_fql: &Fql<hir::TypeDefinition>,
    type_def: &res::TypeDefinition,
    member: &res::TypeDefinitionMember,
) -> MonoType {
    let (hir_module, _) = hir::lower_file(ctx.db, type_def_fql.module_id);

    let type_args: Vec<_> = type_def
        .type_args
        .iter()
        .map(|ty_arg| {
            ctx.get_or_create_annotation_type_var(
                ty_arg.clone(),
                hir_module.get_type_variable(ty_arg.local_id).name.clone(),
            )
        })
        .collect::<Vec<_>>();

    let type_name = type_def_fql.type_def_name(ctx.db);
    let type_var_args: Vec<MonoType> = type_args
        .iter()
        .map(|&var_id| MonoType::Var(var_id))
        .collect();

    let result_type = if type_args.is_empty() {
        MonoType::TypeDef {
            fql: type_def_fql.clone(),
            type_args,
            type_def_name: type_name,
        }
    } else {
        MonoType::App {
            constructor: Box::new(MonoType::TypeDef {
                fql: type_def_fql.clone(),
                type_args,
                type_def_name: type_name,
            }),
            args: type_var_args,
        }
    };

    // Build curried function type: param1 -> (param2 -> (... -> result))
    member
        .properties()
        .iter()
        .map(|type_idx| {
            let annotated = resolve_annotated_type(ctx.db, type_idx.module_id, type_idx.local_id);
            annotated_to_mono(&annotated, ctx).unwrap_or_else(|| ctx.fresh_type_var())
        })
        .rev()
        .fold(result_type, |acc, param_ty| {
            MonoType::Function(Box::new(param_ty), Box::new(acc))
        })
}
