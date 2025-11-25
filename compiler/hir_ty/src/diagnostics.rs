use text_size::TextRange;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeInferenceErrorKind {
    // TODO
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypeInferenceWarning {
    kind: TypeInferenceWarningKind,
    range: TextRange,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeInferenceWarningKind {
    // TODO
}
