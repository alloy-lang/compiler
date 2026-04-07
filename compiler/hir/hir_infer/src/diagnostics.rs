use crate::hir_ty::UnificationError;
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
}

impl Diagnostic for TypeInferenceError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeInferenceErrorKind::UnificationError(_) => Some("E33001"),
            TypeInferenceErrorKind::HirResolutionError(err) => err.code(),
            TypeInferenceErrorKind::UnsatisfiedConstraint { .. } => Some("E33002"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeInferenceErrorKind::UnificationError(unif_err) => match unif_err {
                UnificationError::TypeMismatch(expected, found) => {
                    format!("Type mismatch: expected `{expected}`, found `{found}`",)
                }
                UnificationError::OccursCheck(var, ty) => {
                    format!("Infinite type detected: `{var}` occurs in `{ty}`")
                }
            },
            TypeInferenceErrorKind::HirResolutionError(err) => err.message(),
            TypeInferenceErrorKind::UnsatisfiedConstraint {
                trait_fql_name: trait_name,
                type_name,
                ..
            } => {
                format!("Type `{type_name}` does not implement trait `{trait_name}`")
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeInferenceErrorKind::UnificationError(unif_err) => match unif_err {
                UnificationError::TypeMismatch(expected, found) => builder
                    .with_primary_label(format!("expected `{expected}`, found `{found}`"))
                    .with_help("These types must be compatible"),
                UnificationError::OccursCheck(var, ty) => builder
                    .with_primary_label(format!("type variable `{var}` occurs in `{ty}`"))
                    .with_help("This would create an infinite type, which is not allowed"),
            },
            TypeInferenceErrorKind::HirResolutionError(err) => err.build_report(builder),
            TypeInferenceErrorKind::UnsatisfiedConstraint {
                trait_fql_name: trait_name,
                type_name,
                constraint_range,
            } => builder
                .with_primary_label(format!("missing implementation of trait `{trait_name}`"))
                .with_label(DiagnosticLabel::new(
                    *constraint_range,
                    "required by this constraint",
                ))
                .with_help(format!(
                    "Type `{type_name}` must implement trait `{trait_name}`"
                )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceErrorKind {
    UnificationError(UnificationError),
    HirResolutionError(HirResolutionError),
    UnsatisfiedConstraint {
        trait_fql_name: String,
        type_name: String,
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
