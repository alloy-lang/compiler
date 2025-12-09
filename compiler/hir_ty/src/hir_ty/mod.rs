mod behavior_validation;
mod type_annotation;
mod type_annotation_check;
mod type_definition;

use alloy_hir as hir;
use alloy_hir_resolved::Fql;
use non_empty_vec::NonEmpty;
use std::hash::Hash;

mod hm;
pub use hm::infer_types_hm;
pub use hm::unification::UnificationError;

// Re-export type annotation checking function for use by other modules
pub(super) use type_annotation_check::check_type_annotation;

// ============================================================================
// Type Resolution and Checking
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
    /// Placeholder for unimplemented type system features
    /// Used for TypeReference::SelfRef, MonoType::App, and other TODO cases
    TODO,
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
            | ResolvedType::TypeDef(_)
            | ResolvedType::BuiltIn(_)
            | ResolvedType::TODO => false,
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
            ResolvedType::TypeDef(fql) => write!(f, "TypeDef({})", fql.local_id.into_raw()),
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
                    for (i, constraint) in constraints.iter().enumerate() {
                        if i > 0 {
                            write!(f, " + ")?;
                        }
                        write!(f, "Trait({})", constraint.local_id.into_raw())?;
                    }
                }
                Ok(())
            }
            ResolvedType::TODO => write!(f, "<TODO>"),
        }
    }
}
