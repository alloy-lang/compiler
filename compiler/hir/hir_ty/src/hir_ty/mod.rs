mod behavior_validation;
mod type_annotation_check;

use alloy_hir_def as hir;
use alloy_hir_resolved::Fql;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use std::hash::Hash;

mod hm;
pub use hm::unification::UnificationError;

// Re-export type annotation checking function for use by other modules
use crate::HirTypedModule;
pub(super) use type_annotation_check::check_type_annotation;

pub(super) fn infer_types(db: &dyn crate::HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let mut result = hm::infer_types_hm(db, module_id);

    // Validate that all behaviors implement their trait's abstract members
    behavior_validation::validate_behaviors(db, module_id, &mut result);

    result
}

// ============================================================================
// Resolved Types (output of inference)
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    /// Used when no type annotation is present (sentinel value)
    /// This allows type checking to be skipped for unannotated expressions
    UnknownReference(Fql<hir::TypeReference>),
    /// Explicitly unconstrained type - can match anything (from TypeReference::Unconstrained)
    /// This is used in the AST when a type annotation explicitly says "any type is fine"
    Unconstrained,
    /// Missing type - indicates a syntax error or missing type definition
    /// Used when TypeReference::Missing or TypeDefinitionKind::Missing is encountered
    Missing,
    Unit,
    /// User-defined type (nominal typing)
    /// Two TypeDefs are equal iff they point to the same type definition
    /// The String is the human-readable type name for display purposes
    TypeDef(Fql<hir::TypeDefinition>, hir::Name),
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
    /// Each constraint is a (Fql, name) pair for human-readable display
    ConstrainedGeneric {
        id: usize,
        constraints: NonEmpty<(Fql<hir::Trait>, hir::Name)>,
    },
}

/// Represents a single instantiation of a polymorphic type at a specific call site
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyInstantiation {
    /// The location where the polymorphic value was instantiated (used)
    pub call_site: Fql<hir::Expression>,
    /// The concrete types that each quantified type variable was instantiated to
    /// The order matches the order of quantified variables in the PolyType
    pub type_args: Vec<ResolvedType>,
}

impl ResolvedType {
    /// Check if this type contains type variables (is polymorphic)
    /// Returns true if the type contains Generic or ConstrainedGeneric variants
    pub fn is_polymorphic(&self) -> bool {
        match self {
            ResolvedType::Generic(_) | ResolvedType::ConstrainedGeneric { .. } => true,
            ResolvedType::Lambda {
                arg_type,
                return_type,
            } => arg_type.is_polymorphic() || return_type.is_polymorphic(),
            ResolvedType::Tuple(elements) => elements.iter().any(|e| e.is_polymorphic()),
            ResolvedType::Bounded { base, args } => {
                base.is_polymorphic() || args.iter().any(|a| a.is_polymorphic())
            }
            ResolvedType::UnknownReference(_)
            | ResolvedType::Unconstrained
            | ResolvedType::Missing
            | ResolvedType::Unit
            | ResolvedType::TypeDef(..)
            | ResolvedType::BuiltIn(_) => false,
        }
    }
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::UnknownReference(_) => write!(f, "<unknown>"),
            ResolvedType::Unconstrained => write!(f, "_"),
            ResolvedType::Missing => write!(f, "<missing>"),
            ResolvedType::Unit => write!(f, "()"),
            ResolvedType::TypeDef(_, name) => write!(f, "{name}"),
            ResolvedType::BuiltIn(builtin) => write!(f, "{builtin:?}"),
            ResolvedType::Lambda {
                arg_type,
                return_type,
            } => {
                // Add parentheses if arg_type is also a lambda
                match arg_type.as_ref() {
                    ResolvedType::Lambda { .. } => write!(f, "({arg_type}) -> {return_type}"),
                    _ => write!(f, "{arg_type} -> {return_type}"),
                }
            }
            ResolvedType::Tuple(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            ResolvedType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, "]")
            }
            ResolvedType::Generic(id) => write!(f, "t{id}"),
            ResolvedType::ConstrainedGeneric { id, constraints } => {
                write!(f, "t{id}")?;
                if !constraints.is_empty() {
                    write!(f, " : ")?;
                    for (i, (_, trait_name)) in constraints.iter().enumerate() {
                        if i > 0 {
                            write!(f, " + ")?;
                        }
                        write!(f, "{trait_name}")?;
                    }
                }
                Ok(())
            }
        }
    }
}
