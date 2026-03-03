mod behavior_validation;
mod type_annotation;
mod type_annotation_check;

use alloy_hir as hir;
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
// Annotated Types (what the user wrote in type annotations)
// ============================================================================

/// Represents a resolved type annotation — what the user wrote.
/// Separate from `ResolvedType` (the output of inference).
/// Uses stable HIR identities (`Fql<TypeDefinition>`) for type variables
/// instead of sequential IDs, enabling Salsa-tracked resolution.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnnotatedType {
    Unit,
    BuiltIn(hir::BuiltInType),
    /// Concrete named type (Single/Union type definition)
    TypeDef(Fql<hir::TypeDefinition>, hir::Name),
    /// Function type
    Lambda {
        arg: Box<AnnotatedType>,
        ret: Box<AnnotatedType>,
    },
    /// Tuple type
    Tuple(NonEmpty<AnnotatedType>),
    /// Parameterized type (e.g., List[Int])
    Bounded {
        base: Box<AnnotatedType>,
        args: Vec<AnnotatedType>,
    },
    /// Unconstrained type variable (from typevar declaration)
    TypeVar {
        fql: Fql<hir::TypeDefinition>,
        name: hir::Name,
    },
    /// Constrained type variable (typevar with trait bounds)
    ConstrainedTypeVar {
        fql: Fql<hir::TypeDefinition>,
        name: hir::Name,
        constraints: NonEmpty<(Fql<hir::Trait>, hir::Name)>,
    },
    /// Self type in a trait context
    SelfType {
        trait_fql: Fql<hir::Trait>,
        type_arity: usize,
        trait_constraints: Vec<(Fql<hir::Trait>, hir::Name)>,
    },
    /// Explicitly unconstrained (wildcard)
    Unconstrained,
    /// Syntax error or unresolvable
    Missing,
}

impl AnnotatedType {
    /// Check if this type contains type variables (is polymorphic)
    pub fn is_polymorphic(&self) -> bool {
        match self {
            AnnotatedType::TypeVar { .. }
            | AnnotatedType::ConstrainedTypeVar { .. }
            | AnnotatedType::SelfType { .. } => true,
            AnnotatedType::Lambda { arg, ret } => arg.is_polymorphic() || ret.is_polymorphic(),
            AnnotatedType::Tuple(elements) => elements.iter().any(|e| e.is_polymorphic()),
            AnnotatedType::Bounded { base, args } => {
                base.is_polymorphic() || args.iter().any(|a| a.is_polymorphic())
            }
            AnnotatedType::Unit
            | AnnotatedType::BuiltIn(_)
            | AnnotatedType::TypeDef(..)
            | AnnotatedType::Unconstrained
            | AnnotatedType::Missing => false,
        }
    }
}

impl std::fmt::Display for AnnotatedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotatedType::Unit => write!(f, "()"),
            AnnotatedType::BuiltIn(builtin) => write!(f, "{builtin:?}"),
            AnnotatedType::TypeDef(_, name) => write!(f, "{name}"),
            AnnotatedType::Lambda { arg, ret } => match arg.as_ref() {
                AnnotatedType::Lambda { .. } => write!(f, "({arg}) -> {ret}"),
                _ => write!(f, "{arg} -> {ret}"),
            },
            AnnotatedType::Tuple(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            AnnotatedType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, "]")
            }
            AnnotatedType::TypeVar { name, .. } => write!(f, "{name}"),
            AnnotatedType::ConstrainedTypeVar {
                name, constraints, ..
            } => {
                write!(f, "{name}")?;
                write!(f, " : ")?;
                for (i, (_, trait_name)) in constraints.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{trait_name}")?;
                }
                Ok(())
            }
            AnnotatedType::SelfType {
                trait_constraints: constraints,
                ..
            } => {
                write!(f, "Self")?;
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
            AnnotatedType::Unconstrained => write!(f, "_"),
            AnnotatedType::Missing => write!(f, "<missing>"),
        }
    }
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
