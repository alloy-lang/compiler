use crate::{EPTrFql, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeResolutionError {
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

impl TypeResolutionError {
    pub fn get_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        match self {
            TypeResolutionError::UnresolvedModule { source_ref, .. } => source_ref.text_range(db),
            TypeResolutionError::UnknownExpressionReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownPatternReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownTypeReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownTypeDefinition { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownTypeDefinitionVariant { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::MissingTypeDefinitionVariant { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownTraitReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
            TypeResolutionError::UnknownTraitMember { source_ref, .. } => source_ref.text_range(db),
            TypeResolutionError::BoundedTraitReference { source_ref, .. } => {
                source_ref.text_range(db)
            }
        }
    }
}
