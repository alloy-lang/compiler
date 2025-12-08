use crate::hir_ty::ResolvedType;
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, Severity};
use alloy_hir as hir;
use alloy_hir_resolved::{Fql, TypeResolutionError};
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceError {
    kind: TypeInferenceErrorKind,
    range: TextRange,
}

impl TypeInferenceError {
    #[must_use]
    pub fn new(kind: TypeInferenceErrorKind, range: TextRange) -> Self {
        Self { kind, range }
    }

    /// Get the text range where this error occurred
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// Get the kind of this error
    pub fn kind(&self) -> &TypeInferenceErrorKind {
        &self.kind
    }
}

impl Diagnostic for TypeInferenceError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { .. } => Some("E001"),
            TypeInferenceErrorKind::UnificationError(_) => Some("E002"),
            TypeInferenceErrorKind::TypeResolutionError { .. } => Some("E003"),
            TypeInferenceErrorKind::MissingTraitImplementation { .. } => Some("E004"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { expected, found } => {
                format!(
                    "Type annotation conflict: expected `{}`, found `{}`",
                    expected, found
                )
            }
            TypeInferenceErrorKind::UnificationError(unif_err) => match unif_err {
                crate::hir_ty::UnificationError::TypeMismatch(expected, found) => {
                    format!("Type mismatch: expected `{}`, found `{}`", expected, found)
                }
                crate::hir_ty::UnificationError::OccursCheck(var, ty) => {
                    format!("Infinite type detected: `{}` occurs in `{}`", var, ty)
                }
            },
            TypeInferenceErrorKind::TypeResolutionError(err) => {
                todo!("Type resolution error reporting not implemented yet")
            }
            TypeInferenceErrorKind::MissingTraitImplementation {
                trait_name,
                member_name,
                ..
            } => {
                format!(
                    "Behavior must implement abstract member `{}` from trait `{}`",
                    member_name, trait_name
                )
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { expected, found } => {
                builder
                    .with_primary_label(format!("expected `{}`, found `{}`", expected, found))
                    .with_help(format!(
                        "The type annotation says this should be `{}`, but type inference determined it to be `{}`",
                        expected, found
                    ))
            }
            TypeInferenceErrorKind::UnificationError(unif_err) => {
                match unif_err {
                    crate::hir_ty::UnificationError::TypeMismatch(expected, found) => {
                        builder
                            .with_primary_label(format!("expected `{}`, found `{}`", expected, found))
                            .with_help("These types must be compatible")
                    }
                    crate::hir_ty::UnificationError::OccursCheck(var, ty) => {
                        builder
                            .with_primary_label(format!("type variable `{}` occurs in `{}`", var, ty))
                            .with_help("This would create an infinite type, which is not allowed")
                    }
                }
            }
            TypeInferenceErrorKind::TypeResolutionError(err) => {
                todo!("Type resolution error reporting not implemented yet")
            }
            TypeInferenceErrorKind::MissingTraitImplementation {
                trait_name,
                member_name,
                type_fql,
            } => builder
                .with_primary_label(format!(
                    "missing implementation of `{}`",
                    member_name
                ))
                .with_help(format!(
                    "Trait `{}` requires an implementation of `{}` for type `{:?}`",
                    trait_name, member_name, type_fql
                )),
        }
    }
}

// TODO: Duplicate error reporting issue
//
// Currently, when a type annotation conflicts with the inferred type, we generate both:
// 1. ConflictingTypeAnnotation - High-level error comparing annotation vs inferred type
// 2. UnificationError - Low-level error from the unification algorithm attempting to unify them
//
// This results in redundant error messages for the same underlying issue. The ConflictingTypeAnnotation
// is more specific and user-friendly, so ideally we should suppress the UnificationError in this case.
//
// Potential solutions:
// - Short-term: Deduplicate errors based on range after collection
// - Medium-term: Use ResolvedType::Error sentinel to prevent cascading errors
// - Long-term: Implement proper error recovery strategy that tracks which expressions already have errors
//
// For now, having both errors is useful for debugging the type checker itself, but this should be
// addressed once the type system is more stable.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceErrorKind {
    ConflictingTypeAnnotation {
        expected: ResolvedType,
        found: ResolvedType,
    },
    UnificationError(crate::hir_ty::UnificationError),
    TypeResolutionError(TypeResolutionError),
    MissingTraitImplementation {
        trait_name: String,
        member_name: String,
        type_fql: Fql<hir::TypeDefinition>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceWarning {
    kind: TypeInferenceWarningKind,
    range: TextRange,
}

impl TypeInferenceWarning {
    #[must_use]
    pub fn new(kind: TypeInferenceWarningKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceWarningKind {
    // TODO
}
