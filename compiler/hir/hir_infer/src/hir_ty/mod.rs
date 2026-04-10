use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::Fql;
use itertools::Itertools;
use non_empty_vec::NonEmpty;
use std::hash::Hash;

mod hm;

pub(crate) use hm::infer_body_type;
pub(crate) use hm::infer_expressions;
pub use hm::unification::UnificationError;

/// Display-only metadata that does not participate in equality or hashing.
/// Used to carry human-readable type variable names through to error messages
/// without affecting type identity.
#[derive(Clone)]
pub struct DisplayName(pub String);

impl DisplayName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

impl std::fmt::Debug for DisplayName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

impl PartialEq for DisplayName {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for DisplayName {}

impl Hash for DisplayName {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferredType {
    Unconstrained,
    Missing,
    Unit,
    TypeDef(Fql<hir::TypeDefinition>, hir::Name),
    BuiltIn(hir::BuiltInType),
    Lambda {
        arg_type: Box<InferredType>,
        return_type: Box<InferredType>,
    },
    Tuple(NonEmpty<InferredType>),
    Bounded {
        base: Box<InferredType>,
        args: Vec<InferredType>,
    },
    Generic(usize, DisplayName),
    ConstrainedGeneric {
        id: usize,
        name: DisplayName,
        constraints: NonEmpty<res::TraitConstraint>,
    },
}

impl std::fmt::Display for InferredType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferredType::Unconstrained => write!(f, "_"),
            InferredType::Missing => write!(f, "<missing>"),
            InferredType::Unit => write!(f, "()"),
            InferredType::TypeDef(_, name) => write!(f, "{name}"),
            InferredType::BuiltIn(builtin) => write!(f, "{builtin:?}"),
            InferredType::Lambda {
                arg_type,
                return_type,
            } => {
                // Add parentheses if arg_type is also a lambda
                match arg_type.as_ref() {
                    InferredType::Lambda { .. } => write!(f, "({arg_type}) -> {return_type}"),
                    _ => write!(f, "{arg_type} -> {return_type}"),
                }
            }
            InferredType::Tuple(elements) => {
                write!(f, "(")?;
                elements.iter().join(", ").fmt(f)?;
                write!(f, ")")
            }
            InferredType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                args.iter().join(", ").fmt(f)?;
                write!(f, "]")
            }
            InferredType::Generic(_, DisplayName(name)) => write!(f, "{name}"),
            InferredType::ConstrainedGeneric {
                name: DisplayName(name),
                constraints,
                ..
            } => {
                write!(f, "{name} : ")?;
                constraints
                    .iter()
                    .map(|c| &c.trait_fql_name)
                    .join(" + ")
                    .fmt(f)?;
                Ok(())
            }
        }
    }
}
