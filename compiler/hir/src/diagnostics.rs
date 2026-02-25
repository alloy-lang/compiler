use crate::{HirReferenceType, Import, Name, TypeIdx};
use alloy_scope::ScopeIdx;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LoweringError {
    kind: LoweringErrorKind,
    range: TextRange,
}

impl LoweringError {
    #[must_use]
    pub fn new(kind: LoweringErrorKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum LoweringErrorKind {
    ConflictingValue {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ImportGroupNotAtEnd {
        group_range: TextRange,
        position: usize,
        num_segments: usize,
    },
    ConflictingTypeAnnotationName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingTypeDefinitionName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingTraitDefinitionName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingBehaviorDefinition {
        type_: TypeIdx,
        trait_: TypeIdx,
        first: TextRange,
        second: TextRange,
    },
    ConflictingTypeVariableName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    UnknownReference {
        reference: Name,
        reference_type: HirReferenceType,
        path: NonEmpty<Name>,
        current_scope: ScopeIdx,
    },
    MultipleSelfTypeVariablesInTraitDefinition {
        trait_name: Name,
        ranges: Vec<TextRange>,
    },
    NumberLiteralTooLarge,
    CharLiteralInvalid,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LoweringWarning {
    kind: LoweringWarningKind,
    range: TextRange,
}

impl LoweringWarning {
    #[must_use]
    pub fn new(kind: LoweringWarningKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum LoweringWarningKind {
    DuplicateImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    UnusedImport {
        import: Import,
    },
}
