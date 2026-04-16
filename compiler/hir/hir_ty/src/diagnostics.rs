use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticLabel, Severity};
use alloy_hir_def as hir;
use alloy_hir_infer::InferredType;
use alloy_hir_resolved as res;
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeCheckingErrorKind::ConflictingTypeAnnotation { .. } => Some("E34001"),
            TypeCheckingErrorKind::BoundedTypeArityMismatch { .. } => Some("E34003"),
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
            TypeCheckingErrorKind::BoundedTypeArityMismatch {
                type_name,
                expected_arity,
                actual_arity,
                ..
            } => {
                format!(
                    "Type `{type_name}` expects {expected_arity} type argument(s), but {actual_arity} were provided"
                )
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
                        let builder = builder
                            .with_label(
                                DiagnosticLabel::new(*annotation_range, format!("The type annotation says `{annotated_type}`"))
                            )
                            .with_label(
                                DiagnosticLabel::new(*value_range, format!("but the body has type `{value_type}`"))
                            );

                        // Only show sub-type detail when it points at a narrower
                        // range than the top-level labels — otherwise it's redundant.
                        // Combine into a single label on the inferred side to avoid
                        // splitting the output into multiple source sections.
                        if *actual_type_range != *value_range || *expected_type_range != *annotation_range {
                            builder.with_label(
                                DiagnosticLabel::new(*actual_type_range, format!("this is `{actual_type}`, but the annotation expects `{expected_type}`"))
                            )
                        } else {
                            builder
                        }
                    }
                    ConflictingTypeAnnotationReason::MissingBehaviorImplementation { trait_name, type_name } => {
                        builder
                            .with_primary_label(format!("missing implementation of trait `{trait_name}`"))
                            .with_help(format!("Type `{type_name}` must implement trait `{trait_name}`"))
                    }
                    ConflictingTypeAnnotationReason::InsufficientConstraints { missing_constraints } => {
                        let missing = missing_constraints.iter().map(|(c, _)| format!("`{}`", c.trait_fql_name)).collect::<Vec<_>>().join(", ");

                        builder
                            .with_label(
                                DiagnosticLabel::new(*annotation_range, format!("type annotation is missing required constraint(s): {missing}"))
                            )
                            .with_label(
                                DiagnosticLabel::new(*value_range, format!("but the body requires {missing}"))
                            )
                            .with_help(format!("Add the missing constraint(s) to the type annotation: {missing}"))
                    }
                }
            }
            TypeCheckingErrorKind::BoundedTypeArityMismatch {
                type_name,
                expected_arity,
                actual_arity,
                annotation_range,
            } => builder.with_label(DiagnosticLabel::new(
                *annotation_range,
                format!(
                    "`{type_name}` expects {expected_arity} type argument(s), but {actual_arity} were provided"
                ),
            )),
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

    fn is_hidden_by(&self, other: &dyn Diagnostic) -> bool {
        if Some(self) == other.as_any().downcast_ref::<Self>() {
            return true;
        }

        // Cross-phase: wrapped inference/resolution errors hidden by parse errors
        if matches!(
            &self.kind,
            TypeCheckingErrorKind::InferenceError(_) | TypeCheckingErrorKind::HirResolutionError(_)
        ) && other.as_any().is::<alloy_parser::ParseError>()
        {
            return self.overlaps_or_contains(other);
        }

        // Cross-phase: wrapped inference/resolution errors hidden by lowering errors
        // at the same range (the lowering error is the root cause)
        if matches!(
            &self.kind,
            TypeCheckingErrorKind::InferenceError(_) | TypeCheckingErrorKind::HirResolutionError(_)
        ) && other.as_any().is::<hir::LoweringError>()
        {
            return self.overlaps_with(other);
        }

        if !self.overlaps_with(other) {
            return false;
        }

        // Intra-phase: downcast to TypeCheckingError for kind-level matching
        let Some(other) = other.as_any().downcast_ref::<Self>() else {
            return false;
        };
        match (&self.kind, &other.kind) {
            // Wrapped TypeMismatch hidden by ConflictingTypeAnnotation or BoundedTypeArityMismatch
            (
                TypeCheckingErrorKind::InferenceError(ie),
                TypeCheckingErrorKind::ConflictingTypeAnnotation { .. }
                | TypeCheckingErrorKind::BoundedTypeArityMismatch { .. },
            ) if matches!(
                ie.kind(),
                alloy_hir_infer::TypeInferenceErrorKind::TypeMismatch { .. }
            ) =>
            {
                true
            }
            // ConflictingTypeAnnotation hidden by BoundedTypeArityMismatch
            (
                TypeCheckingErrorKind::ConflictingTypeAnnotation { .. },
                TypeCheckingErrorKind::BoundedTypeArityMismatch { .. },
            ) => true,
            _ => false,
        }
    }
}

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
    BoundedTypeArityMismatch {
        type_name: hir::Name,
        expected_arity: usize,
        actual_arity: usize,
        annotation_range: TextRange,
    },
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
    InsufficientConstraints {
        missing_constraints: Vec<(res::TraitConstraint, TextRange)>,
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
