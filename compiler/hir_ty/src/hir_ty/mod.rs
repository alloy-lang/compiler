mod type_annotation_check;
mod type_definition;
mod type_reference;

use alloy_hir as hir;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;
use std::hash::Hash;

// Re-export type annotation checking function for use by other modules
pub(super) use type_annotation_check::check_type_annotation;

/// Fully Qualified Location - represents an index within a specific module
#[derive(Debug, Clone, Copy)]
pub struct Fql<T> {
    pub module_id: ModuleId,
    pub local_id: Idx<T>,
}

impl<T> Fql<T> {
    pub fn new(module_id: ModuleId, local_id: impl Into<Idx<T>>) -> Self {
        Self {
            module_id,
            local_id: local_id.into(),
        }
    }
}

impl<T> PartialEq for Fql<T> {
    fn eq(&self, other: &Self) -> bool {
        self.module_id == other.module_id && self.local_id == other.local_id
    }
}

impl<T> Eq for Fql<T> {}

impl<T> Hash for Fql<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module_id.hash(state);
        self.local_id.hash(state);
    }
}

// ============================================================================
// Hindley-Milner Type System
// ============================================================================

mod hm;
pub use hm::infer_types_hm;

// ============================================================================
// Shared Types
// ============================================================================

/// A fully qualified reference to an expression or pattern within a specific module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum ExpressionOrPatternIdx {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
}

// ============================================================================
// Type Resolution and Checking
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Unit,
    /// User-defined type (nominal typing)
    /// Two TypeDefs are equal iff they point to the same type definition
    TypeDef(Fql<hir::TypeDefinition>),
    BuiltIn(hir::BuiltInType),
    Lambda {
        arg_type: Box<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Tuple(NonEmpty<ResolvedType>),
    Bounded {
        base: Box<ResolvedType>,
        args: Vec<ResolvedType>,
    },
    /// Unconstrained generic type variable (for unbounded polymorphism)
    /// The usize represents a canonical type variable ID
    Generic(usize),
    /// Generic type variable with trait constraints (for bounded polymorphism)
    /// The usize represents a canonical type variable ID
    /// The NonEmpty contains trait constraints this generic must satisfy
    ConstrainedGeneric {
        id: usize,
        constraints: NonEmpty<Fql<hir::Trait>>,
    },
}
