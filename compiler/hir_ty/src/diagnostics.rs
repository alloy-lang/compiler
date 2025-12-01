use crate::hir_ty::ResolvedType;
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceErrorKind {
    ConflictingTypeAnnotation {
        expected: ResolvedType,
        found: ResolvedType,
    },
    UnificationError(crate::hir_ty::UnificationError),
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
