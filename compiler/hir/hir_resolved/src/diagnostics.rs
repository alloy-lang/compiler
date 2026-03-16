use crate::{EPTrFql, Fql};
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, Severity};
use alloy_hir_def as hir;
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirResolutionError {
    UnresolvedModule {
        source_ref: EPTrFql,
        err: hir::FqnResolutionError,
    },
    UnknownExpressionReference {
        source_ref: Fql<hir::Expression>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownPatternReference {
        source_ref: Fql<hir::Pattern>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownTypeReference {
        source_ref: Fql<hir::TypeReference>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownTypeDefinition {
        source_ref: Fql<hir::TypeReference>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownTypeDefinitionVariant {
        source_ref: EPTrFql,
        target_type_fql: Fql<hir::TypeDefinition>,
        variant_name: hir::Name,
    },
    MissingTypeDefinitionVariant {
        source_ref: EPTrFql,
        target_type_fql: Fql<hir::TypeDefinition>,
    },
    UnknownTraitReference {
        source_ref: EPTrFql,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownTraitMember {
        source_ref: EPTrFql,
        module_id: ModuleId,
        trait_idx: hir::TraitIdx,
        subname: hir::Name,
    },
    BoundedTraitReference {
        source_ref: Fql<hir::TypeReference>,
        target_ref: Fql<hir::TypeReference>,
    },
}

impl HirResolutionError {
    pub fn get_range(&self, db: &dyn hir::HirDefDatabase) -> TextRange {
        match self {
            HirResolutionError::UnresolvedModule { source_ref, .. }
            | HirResolutionError::UnknownTypeDefinitionVariant { source_ref, .. }
            | HirResolutionError::MissingTypeDefinitionVariant { source_ref, .. }
            | HirResolutionError::UnknownTraitReference { source_ref, .. }
            | HirResolutionError::UnknownTraitMember { source_ref, .. } => {
                source_ref.text_range(db)
            }
            HirResolutionError::UnknownExpressionReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            HirResolutionError::UnknownPatternReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            HirResolutionError::UnknownTypeReference { source_ref, .. }
            | HirResolutionError::UnknownTypeDefinition { source_ref, .. }
            | HirResolutionError::BoundedTraitReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
        }
    }

    fn path_to_slug(path: &NonEmpty<hir::Name>) -> String {
        path.iter().map(hir::Name::as_str).join("::")
    }
}

impl Diagnostic for HirResolutionError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match self {
            HirResolutionError::UnresolvedModule { err, .. } => err.code(),
            HirResolutionError::UnknownExpressionReference { .. } => Some("E32002"),
            HirResolutionError::UnknownPatternReference { .. } => Some("E32003"),
            HirResolutionError::UnknownTypeReference { .. } => Some("E32004"),
            HirResolutionError::UnknownTypeDefinition { .. } => Some("E32005"),
            HirResolutionError::UnknownTypeDefinitionVariant { .. } => Some("E32006"),
            HirResolutionError::MissingTypeDefinitionVariant { .. } => Some("E32007"),
            HirResolutionError::UnknownTraitReference { .. } => Some("E32008"),
            HirResolutionError::UnknownTraitMember { .. } => Some("E32009"),
            HirResolutionError::BoundedTraitReference { .. } => Some("E32010"),
        }
    }

    fn message(&self) -> String {
        match self {
            HirResolutionError::UnresolvedModule { err, .. } => err.message(),
            HirResolutionError::UnknownExpressionReference { path, .. } => {
                let path_str = Self::path_to_slug(path);
                format!("Cannot find value `{path_str}` in this scope")
            }
            HirResolutionError::UnknownPatternReference { path, .. } => {
                let path_str = Self::path_to_slug(path);
                format!("Cannot find pattern `{path_str}` in this scope")
            }
            HirResolutionError::UnknownTypeReference { path, .. } => {
                let path_str = Self::path_to_slug(path);
                format!("Cannot find type `{path_str}` in this scope")
            }
            HirResolutionError::UnknownTypeDefinition { path, .. } => {
                let path_str = Self::path_to_slug(path);
                format!("Cannot find type definition `{path_str}`")
            }
            HirResolutionError::UnknownTypeDefinitionVariant { variant_name, .. } => {
                format!("Unknown variant `{}`", variant_name.as_str())
            }
            HirResolutionError::MissingTypeDefinitionVariant { .. } => {
                "Missing variant name for multi-variant type".to_string()
            }
            HirResolutionError::UnknownTraitReference { path, .. } => {
                let path_str = Self::path_to_slug(path);
                format!("Cannot find trait `{path_str}`")
            }
            HirResolutionError::UnknownTraitMember { subname, .. } => {
                format!("Cannot find trait member `{subname}`")
            }
            HirResolutionError::BoundedTraitReference { .. } => {
                "Bounded trait reference resolution not yet implemented".to_string()
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        unreachable!("caller should track range")
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match self {
            HirResolutionError::UnresolvedModule { err, .. } => err.build_report(builder),
            HirResolutionError::UnknownExpressionReference {
                path, module_id, ..
            } => {
                let path_str = Self::path_to_slug(path);
                let module_slug = builder.format_module_slug(module_id);
                builder
                    .with_primary_label(format!("cannot find `{path_str}`"))
                    .with_help(format!(
                        "No value named `{path_str}` found in module `{module_slug}`"
                    ))
            }
            HirResolutionError::UnknownPatternReference {
                path, module_id, ..
            } => {
                let path_str = Self::path_to_slug(path);
                let module_slug = builder.format_module_slug(module_id);
                builder
                    .with_primary_label(format!("cannot find pattern `{path_str}`"))
                    .with_help(format!(
                        "No pattern named `{path_str}` found in module `{module_slug}`",
                    ))
            }
            HirResolutionError::UnknownTypeReference {
                path, module_id, ..
            } => {
                let path_str = Self::path_to_slug(path);
                let module_slug = builder.format_module_slug(module_id);
                builder
                    .with_primary_label(format!("cannot find type `{path_str}`"))
                    .with_help(format!(
                        "No type named `{path_str}` found in module `{module_slug}`"
                    ))
            }
            HirResolutionError::UnknownTypeDefinition {
                path, module_id, ..
            } => {
                let path_str = Self::path_to_slug(path);
                let module_slug = builder.format_module_slug(module_id);
                builder
                    .with_primary_label(format!("cannot find type definition `{path_str}`"))
                    .with_help(format!(
                        "No type definition named `{path_str}` found in module `{module_slug}`"
                    ))
            }
            HirResolutionError::UnknownTypeDefinitionVariant { variant_name, .. } => builder
                .with_primary_label(format!("variant `{variant_name}` not found"))
                .with_help(format!(
                    "No variant named `{variant_name}` exists on this type"
                )),
            HirResolutionError::MissingTypeDefinitionVariant { .. } => builder
                .with_primary_label("missing variant name")
                .with_help(
                    "Multi-variant types require specifying a variant (e.g., `Type::Variant`)",
                ),
            HirResolutionError::UnknownTraitReference {
                path, module_id, ..
            } => {
                let path_str = Self::path_to_slug(path);
                let module_slug = builder.format_module_slug(module_id);
                builder
                    .with_primary_label(format!("cannot find trait `{path_str}`"))
                    .with_help(format!(
                        "No trait named `{path_str}` found in module `{module_slug}`"
                    ))
            }
            HirResolutionError::UnknownTraitMember { subname, .. } => builder
                .with_primary_label(format!("cannot find trait member `{subname}`"))
                .with_help("Check that the trait name is correct and the module is imported"),
            HirResolutionError::BoundedTraitReference { .. } => builder
                .with_primary_label("bounded trait reference")
                .with_help("Bounded trait references are not yet fully implemented"),
        }
    }
}
