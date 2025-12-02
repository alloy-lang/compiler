//! Constraint generation for expressions and patterns
//!
//! This module walks through expressions and patterns, generating type equations
//! and assigning types to each node.
//!
//! # Outstanding TODOs:
//!
//! 1. When implementing a trait with a 'behavior', we need to verify that all abstract
//!    trait members are implemented. Currently there's no validation that all required
//!    methods are provided when implementing a trait.
//!
//! 2. Resolve trait functions that have been implemented for type definitions. For example,
//!    when both `typedef std::option::Option` and `trait std::monad::Monad` are imported,
//!    we should be able to resolve `Option::flat_map` to the trait implementation.
//!    This requires tracking which traits are implemented for which types and providing
//!    those methods in the namespace.

mod expr;
mod expr_function_call;
mod expr_variable_reference;
mod pattern;

use alloy_hir as hir;
use alloy_workspace::ModuleId;

use super::super::ExpressionOrPatternIdx;
use super::{HMInferenceContext, MonoType};

pub(crate) use expr::infer_expr_hm;

fn infer_literal(
    ctx: &mut HMInferenceContext,
    idx: ExpressionOrPatternIdx,
    lit: &hir::Literal,
) -> MonoType {
    let ty = MonoType::Concrete(hir::BuiltInType::from(lit));
    ctx.assign_type(idx, ty)
}

fn infer_unit(ctx: &mut HMInferenceContext, idx: ExpressionOrPatternIdx) -> MonoType {
    let ty = MonoType::Unit;
    ctx.assign_type(idx, ty)
}

/// Helper function to resolve a cross-module expression reference
fn resolve_cross_module_expression(
    ctx: &HMInferenceContext,
    fqn: &hir::Fqn,
) -> Option<(ModuleId, hir::ExpressionIdx)> {
    let module_slug = fqn.module_slug();
    let other_module_id = ctx.db.find_module_by_slug(&*module_slug)?;
    let (hir_module, _) = hir::lower_file(ctx.db, other_module_id);
    let (expr_id, _) = hir_module.get_expression_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
    Some((other_module_id, expr_id))
}

/// Helper function to resolve a cross-module pattern reference
fn resolve_cross_module_pattern(
    ctx: &HMInferenceContext,
    fqn: &hir::Fqn,
) -> Option<(ModuleId, hir::PatternIdx)> {
    let module_slug = fqn.module_slug();
    let other_module_id = ctx.db.find_module_by_slug(&*module_slug)?;
    let (hir_module, _) = hir::lower_file(ctx.db, other_module_id);
    let (pat_id, _) = hir_module.get_pattern_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
    Some((other_module_id, pat_id))
}
