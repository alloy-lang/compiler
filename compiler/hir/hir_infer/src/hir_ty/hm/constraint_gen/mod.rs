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

use super::{HMInferenceContext, MonoType};

use alloy_hir_resolved::EPTdFql;
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
