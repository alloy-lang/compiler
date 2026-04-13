use crate::hir_ty::{InferredType, TypeVarId};
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticLabel, Severity};
use alloy_hir_resolved::HirResolutionError;
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

    #[must_use]
    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn kind(&self) -> &TypeInferenceErrorKind {
        &self.kind
    }
}

impl Diagnostic for TypeInferenceError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeInferenceErrorKind::InfiniteType { .. } => Some("E33003"),
            TypeInferenceErrorKind::TypeMismatch { .. } => Some("E33001"),
            TypeInferenceErrorKind::HirResolutionError(err) => err.code(),
            TypeInferenceErrorKind::UnsatisfiedConstraint { .. } => Some("E33002"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeInferenceErrorKind::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected `{expected}`, found `{found}`",)
            }
            TypeInferenceErrorKind::InfiniteType {
                type_var_id,
                inferred_type,
            } => {
                format!("Infinite type detected: `{type_var_id}` occurs in `{inferred_type}`")
            }
            TypeInferenceErrorKind::HirResolutionError(err) => err.message(),
            TypeInferenceErrorKind::UnsatisfiedConstraint {
                trait_fql_name: trait_name,
                resolved_type,
                ..
            } => {
                format!("Type `{resolved_type}` does not implement trait `{trait_name}`")
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeInferenceErrorKind::TypeMismatch { expected, found } => builder
                .with_primary_label(format!("expected `{expected}`, found `{found}`"))
                .with_help("These types must be compatible"),
            TypeInferenceErrorKind::InfiniteType {
                type_var_id,
                inferred_type,
            } => builder
                .with_primary_label(format!(
                    "type variable `{type_var_id}` occurs in `{inferred_type}`"
                ))
                .with_help("This would create an infinite type, which is not allowed"),
            TypeInferenceErrorKind::HirResolutionError(err) => err.build_report(builder),
            TypeInferenceErrorKind::UnsatisfiedConstraint {
                trait_fql_name: trait_name,
                resolved_type,
                constraint_range,
            } => builder
                .with_primary_label(format!("missing implementation of trait `{trait_name}`"))
                .with_label(DiagnosticLabel::new(
                    *constraint_range,
                    "required by this constraint",
                ))
                .with_help(format!(
                    "Type `{resolved_type}` must implement trait `{trait_name}`"
                )),
        }
    }

    fn is_hidden_by(&self, other: &dyn Diagnostic) -> bool {
        if !self.overlaps_with(other) {
            return false;
        }
        // Inference errors are hidden by parse errors at overlapping ranges
        other.as_any().is::<alloy_parser::ParseError>()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceErrorKind {
    // OccursCheck
    InfiniteType {
        type_var_id: TypeVarId,
        inferred_type: InferredType,
    },
    TypeMismatch {
        expected: InferredType,
        found: InferredType,
    },
    HirResolutionError(HirResolutionError),
    UnsatisfiedConstraint {
        trait_fql_name: String,
        resolved_type: InferredType,
        constraint_range: TextRange,
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

    #[must_use]
    pub fn range(&self) -> TextRange {
        self.range
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceWarningKind {
    // TODO
}
