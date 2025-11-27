mod type_definition;
mod type_reference;

use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir::Name;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;
use std::hash::Hash;
use text_size::TextRange;

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

pub(super) fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    name_op: Option<(Name, ScopeIdx)>,
    resolved_type: ResolvedType,
) {
    // Check for type annotation conflicts
    if let Some((name, scope)) = name_op {
        let expected_type = type_reference::type_reference_to_resolved(
            db,
            current_module_id,
            &hir::Path::ThisModule {
                path: NonEmpty::new(name.clone()),
                scope,
            },
            scope,
        );
        if expected_type != ResolvedType::Unknown && expected_type != resolved_type {
            result.error(
                crate::diagnostics::TypeInferenceErrorKind::ConflictingTypeAnnotation {
                    expected: expected_type,
                    found: resolved_type,
                },
                range,
            );
        }
    }
}
