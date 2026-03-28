use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticLabel, Severity};
use alloy_hir_def as hir;
use alloy_hir_infer::InferredType;
use alloy_hir_resolved::{AnnotatedType, HirResolutionError};
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckingError {
    kind: TypeCheckingErrorKind,
    range: TextRange,
}

impl TypeCheckingError {
    #[must_use]
    pub fn new(kind: TypeCheckingErrorKind, range: TextRange) -> Self {
        Self { kind, range }
    }

    /// Get the text range where this error occurred
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// Get the kind of this error
    pub fn kind(&self) -> &TypeCheckingErrorKind {
        &self.kind
    }
}

impl From<&alloy_hir_infer::TypeInferenceError> for TypeCheckingError {
    fn from(warning: &alloy_hir_infer::TypeInferenceError) -> Self {
        let range = warning.range();
        Self {
            kind: TypeCheckingErrorKind::InferenceError(warning.clone()),
            range,
        }
    }
}

impl Diagnostic for TypeCheckingError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeCheckingErrorKind::ConflictingTypeAnnotation { .. } => Some("E34001"),
            TypeCheckingErrorKind::InferenceError(err) => err.code(),
            TypeCheckingErrorKind::HirResolutionError(err) => err.code(),
            TypeCheckingErrorKind::MissingTraitMemberImplementation { .. } => Some("E34002"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeCheckingErrorKind::ConflictingTypeAnnotation { .. } => {
                "Inferred type does not match annotated type".to_string()
            }
            TypeCheckingErrorKind::InferenceError(err) => err.message(),
            TypeCheckingErrorKind::HirResolutionError(err) => err.message(),
            TypeCheckingErrorKind::MissingTraitMemberImplementation {
                trait_name,
                member_name,
                ..
            } => {
                format!(
                    "Behavior must implement abstract member `{member_name}` from trait `{trait_name}`"
                )
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeCheckingErrorKind::ConflictingTypeAnnotation { annotated_type, annotation_range, value_type, value_range, reason,  } => {
                match reason {
                    ConflictingTypeAnnotationReason::DirectConflict { expected_type, expected_type_range, actual_type, actual_type_range } => {
                        builder
                            .with_label(
                                DiagnosticLabel::new(*annotation_range, format!("The type annotation here says `{annotated_type}`"))
                            )
                            .with_label(
                                DiagnosticLabel::new(*value_range, format!("but type inference determined this to be `{value_type}`"))
                            )
                            .with_label(
                                DiagnosticLabel::new(*actual_type_range, format!("Specifically, the annotation expects `{actual_type}`"))
                            )
                            .with_label(
                                DiagnosticLabel::new(*expected_type_range, format!("to be `{expected_type}`"))
                            )
                    }
                    ConflictingTypeAnnotationReason::MissingBehaviorImplementation { trait_name, type_name } => {
                        builder
                            .with_primary_label(format!("missing implementation of trait `{trait_name}`"))
                            .with_help(format!("Type `{type_name}` must implement trait `{trait_name}`"))
                    }
                }
            }
            TypeCheckingErrorKind::InferenceError(err) => err.build_report(builder),
            TypeCheckingErrorKind::HirResolutionError(err) => err.build_report(builder),
            TypeCheckingErrorKind::MissingTraitMemberImplementation {
                trait_name,
                member_name,
                type_name,
            } => builder
                .with_primary_label(format!(
                    "missing implementation of `{member_name}`"
                ))
                .with_help(format!(
                    "Trait `{trait_name}` requires an implementation of `{member_name}` for type `{type_name}`",
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
// - Medium-term: Use InferredType::Error sentinel to prevent cascading errors
// - Long-term: Implement proper error recovery strategy that tracks which expressions already have errors
//
// For now, having both errors is useful for debugging the type checker itself, but this should be
// addressed once the type system is more stable.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckingErrorKind {
    ConflictingTypeAnnotation {
        annotated_type: AnnotatedType,
        annotation_range: TextRange,
        value_type: InferredType,
        value_range: TextRange,
        reason: ConflictingTypeAnnotationReason,
    },
    InferenceError(alloy_hir_infer::TypeInferenceError),
    HirResolutionError(HirResolutionError),
    MissingTraitMemberImplementation {
        trait_name: hir::Name,
        member_name: hir::Name,
        type_name: hir::Name,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConflictingTypeAnnotationReason {
    DirectConflict {
        expected_type: AnnotatedType,
        expected_type_range: TextRange,
        actual_type: InferredType,
        actual_type_range: TextRange,
    },
    MissingBehaviorImplementation {
        trait_name: hir::Name,
        type_name: hir::Name,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckingWarning {
    kind: TypeCheckingWarningKind,
    range: TextRange,
}

impl TypeCheckingWarning {
    #[must_use]
    pub fn new(kind: TypeCheckingWarningKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

impl From<&alloy_hir_infer::TypeInferenceWarning> for TypeCheckingWarning {
    fn from(warning: &alloy_hir_infer::TypeInferenceWarning) -> Self {
        let range = warning.range();
        Self {
            kind: TypeCheckingWarningKind::InferenceWarning(warning.clone()),
            range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCheckingWarningKind {
    InferenceWarning(alloy_hir_infer::TypeInferenceWarning),
}
