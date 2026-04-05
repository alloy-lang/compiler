use crate::fqn::FqnResolutionError;
use crate::{HirReferenceType, Import, Name, TypeIdx};
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticLabel, Severity};
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

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn kind(&self) -> &LoweringErrorKind {
        &self.kind
    }
}

impl Diagnostic for LoweringError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            LoweringErrorKind::ConflictingValue { .. } => Some("E22001"),
            LoweringErrorKind::ConflictingImport { .. } => Some("E22002"),
            LoweringErrorKind::ImportGroupNotAtEnd { .. } => Some("E22003"),
            LoweringErrorKind::ConflictingTypeAnnotationName { .. } => Some("E22004"),
            LoweringErrorKind::ConflictingTypeDefinitionName { .. } => Some("E22005"),
            LoweringErrorKind::ConflictingTraitDefinitionName { .. } => Some("E22006"),
            LoweringErrorKind::ConflictingBehaviorDefinition { .. } => Some("E22007"),
            LoweringErrorKind::ConflictingTypeVariableName { .. } => Some("E22008"),
            LoweringErrorKind::UnknownReference { .. } => Some("E22009"),
            LoweringErrorKind::FailedModuleResolution { fqn_error, .. } => fqn_error.code(),
            LoweringErrorKind::MultipleSelfTypeVariablesInTraitDefinition { .. } => Some("E22010"),
            LoweringErrorKind::NumberLiteralTooLarge => Some("E22011"),
            LoweringErrorKind::CharLiteralInvalid => Some("E22012"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            LoweringErrorKind::ConflictingValue { name, .. } => {
                format!("Conflicting definition for `{name}`")
            }
            LoweringErrorKind::ConflictingImport { name, .. } => {
                format!("Conflicting import for `{name}`")
            }
            LoweringErrorKind::ImportGroupNotAtEnd { .. } => {
                "Import group must be the last segment in an import path".to_string()
            }
            LoweringErrorKind::ConflictingTypeAnnotationName { name, .. } => {
                format!("Conflicting type annotation for `{name}`")
            }
            LoweringErrorKind::ConflictingTypeDefinitionName { name, .. } => {
                format!("Conflicting type definition for `{name}`")
            }
            LoweringErrorKind::ConflictingTraitDefinitionName { name, .. } => {
                format!("Conflicting trait definition for `{name}`")
            }
            LoweringErrorKind::ConflictingBehaviorDefinition { .. } => {
                "Conflicting behavior definition".to_string()
            }
            LoweringErrorKind::ConflictingTypeVariableName { name, .. } => {
                format!("Conflicting type variable `{name}`")
            }
            LoweringErrorKind::UnknownReference {
                reference,
                reference_type,
                ..
            } => {
                let kind = match reference_type {
                    HirReferenceType::Expression => "value",
                    HirReferenceType::Pattern => "pattern",
                    HirReferenceType::Type => "type",
                };
                format!("Unknown {kind} reference `{reference}`")
            }
            LoweringErrorKind::FailedModuleResolution { fqn_error, .. } => fqn_error.message(),
            LoweringErrorKind::MultipleSelfTypeVariablesInTraitDefinition {
                trait_name, ..
            } => {
                format!("Multiple `self` type variables in trait `{trait_name}`")
            }
            LoweringErrorKind::NumberLiteralTooLarge => "Number literal is too large".to_string(),
            LoweringErrorKind::CharLiteralInvalid => "Invalid character literal".to_string(),
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            LoweringErrorKind::ConflictingValue {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("`{name}` first defined here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("`{name}` redefined here"),
                )),
            LoweringErrorKind::ConflictingImport {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("`{name}` first imported here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("`{name}` imported again here"),
                )),
            LoweringErrorKind::ImportGroupNotAtEnd {
                group_range,
                position,
                num_segments,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *group_range,
                    format!("import group at position {position} of {num_segments}, must be last"),
                ))
                .with_help("Move the import group `{{...}}` to the end of the import path"),
            LoweringErrorKind::ConflictingTypeAnnotationName {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("type annotation for `{name}` first defined here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("conflicting type annotation for `{name}`"),
                )),
            LoweringErrorKind::ConflictingTypeDefinitionName {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("type `{name}` first defined here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("type `{name}` redefined here"),
                )),
            LoweringErrorKind::ConflictingTraitDefinitionName {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("trait `{name}` first defined here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("trait `{name}` redefined here"),
                )),
            LoweringErrorKind::ConflictingBehaviorDefinition { first, second, .. } => builder
                .with_label(DiagnosticLabel::new(*first, "behavior first defined here"))
                .with_label(DiagnosticLabel::new(
                    *second,
                    "conflicting behavior definition",
                )),
            LoweringErrorKind::ConflictingTypeVariableName {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("type variable `{name}` first declared here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    format!("type variable `{name}` redeclared here"),
                )),
            LoweringErrorKind::UnknownReference {
                reference,
                reference_type,
                ..
            } => {
                let kind = match reference_type {
                    HirReferenceType::Expression => "value",
                    HirReferenceType::Pattern => "pattern",
                    HirReferenceType::Type => "type",
                };
                builder
                    .with_primary_label(format!("cannot find {kind} `{reference}` in this scope"))
            }
            LoweringErrorKind::FailedModuleResolution { fqn_error, .. } => {
                fqn_error.build_report(builder)
            }
            LoweringErrorKind::MultipleSelfTypeVariablesInTraitDefinition { ranges, .. } => {
                let mut b = builder;
                for (i, range) in ranges.iter().enumerate() {
                    let msg = if i == 0 {
                        "first `self` type variable here".to_string()
                    } else {
                        "additional `self` type variable here".to_string()
                    };
                    b = b.with_label(DiagnosticLabel::new(*range, msg));
                }
                b.with_help("A trait can only have one `self` type variable")
            }
            LoweringErrorKind::NumberLiteralTooLarge => {
                builder.with_primary_label("number literal exceeds maximum value")
            }
            LoweringErrorKind::CharLiteralInvalid => {
                builder.with_primary_label("invalid character literal")
            }
        }
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
    FailedModuleResolution {
        reference_type: HirReferenceType,
        fqn_error: FqnResolutionError,
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

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn kind(&self) -> &LoweringWarningKind {
        &self.kind
    }
}

impl Diagnostic for LoweringWarning {
    fn severity(&self) -> Severity {
        Severity::Warning
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            LoweringWarningKind::DuplicateImport { .. } => Some("W22001"),
            LoweringWarningKind::UnusedImport { .. } => Some("W22002"),
            LoweringWarningKind::MissingDefinition { .. } => Some("W22003"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            LoweringWarningKind::DuplicateImport { name, .. } => {
                format!("Duplicate import of `{name}`")
            }
            LoweringWarningKind::UnusedImport { import } => {
                format!("Unused import `{}`", import.last())
            }
            LoweringWarningKind::MissingDefinition { name } => {
                format!("Type annotation `{name}` has no corresponding definition")
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            LoweringWarningKind::DuplicateImport {
                name,
                first,
                second,
            } => builder
                .with_label(DiagnosticLabel::new(
                    *first,
                    format!("`{name}` first imported here"),
                ))
                .with_label(DiagnosticLabel::new(
                    *second,
                    "duplicate import".to_string(),
                )),
            LoweringWarningKind::UnusedImport { import } => {
                builder.with_primary_label(format!("unused import `{}`", import.last()))
            }
            LoweringWarningKind::MissingDefinition { name } => builder
                .with_primary_label(format!("type annotation `{name}` has no definition"))
                .with_help(format!("Add a `let {name} = ...` definition")),
        }
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
    MissingDefinition {
        name: Name,
    },
}
