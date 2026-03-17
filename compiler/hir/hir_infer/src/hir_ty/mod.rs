use crate::HirInferredModule;
use alloy_hir_def as hir;
use alloy_hir_resolved::{AnnotatedType, EPFql, Fql, HirResolutionError};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use std::hash::Hash;

mod hm;
pub use hm::unification::UnificationError;

pub(super) fn infer_types(
    db: &dyn crate::HirInferDatabase,
    module_id: ModuleId,
) -> HirInferredModule {
    hm::infer_types_hm(db, module_id)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferredType {
    // ResolutionError(HirResolutionError),
    // Annotated(AnnotatedType),
    // Variable(EPFql),
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
    Generic(usize),
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
    pub type_args: Vec<InferredType>,
}

impl InferredType {
    pub fn is_polymorphic(&self) -> bool {
        match self {
            InferredType::Generic(_) | InferredType::ConstrainedGeneric { .. } => true,
            // InferredType::Annotated(annot) => annot.is_polymorphic(),
            InferredType::Lambda {
                arg_type,
                return_type,
            } => arg_type.is_polymorphic() || return_type.is_polymorphic(),
            InferredType::Tuple(elements) => elements.iter().any(|e| e.is_polymorphic()),
            InferredType::Bounded { base, args } => {
                base.is_polymorphic() || args.iter().any(|a| a.is_polymorphic())
            }
            // InferredType::ResolutionError(_)
            // |
            InferredType::Unconstrained
            | InferredType::Missing
            | InferredType::Unit
            | InferredType::TypeDef(..)
            | InferredType::BuiltIn(_) => false,
        }
    }
}

impl std::fmt::Display for InferredType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // InferredType::ResolutionError(_) => write!(f, "<error>"),
            // InferredType::Annotated(annot) => write!(f, "{annot}"),
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
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            InferredType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, "]")
            }
            InferredType::Generic(id) => write!(f, "t{id}"),
            InferredType::ConstrainedGeneric { id, constraints } => {
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
