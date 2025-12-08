use crate::{EPTFql, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeResolutionError {
    UnknownModule {
        source_ref: EPTFql,
        module_slug: String,
    },
    UnknownTraitModule {
        source_ref: Fql<hir::TypeReference>,
        module_slug: String,
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
    UnknownTraitReference {
        source_ref: Fql<hir::TypeReference>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    UnknownTraitName {
        source_ref: Fql<hir::TypeReference>,
        fqn: hir::Fqn,
    },
    BoundedTraitReference {
        source_ref: Fql<hir::TypeReference>,
        target_ref: Fql<hir::TypeReference>,
    },
}

impl TypeResolutionError {
    pub fn get_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        match self {
            TypeResolutionError::UnknownModule { source_ref, .. } => match source_ref {
                EPTFql::Expression(fql) => {
                    let (hir_module, _) = hir::lower_file(db, fql.module_id);
                    hir_module.get_expression_range(fql.local_id)
                }
                EPTFql::Pattern(fql) => {
                    let (hir_module, _) = hir::lower_file(db, fql.module_id);
                    hir_module.get_pattern_range(fql.local_id)
                }
                EPTFql::TypeReference(fql) => {
                    let (hir_module, _) = hir::lower_file(db, fql.module_id);
                    hir_module.get_type_reference_range(fql.local_id)
                }
            },
            TypeResolutionError::UnknownExpressionReference { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_expression_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownPatternReference { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_pattern_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownTraitModule { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownTypeReference { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownTypeDefinition { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownTraitReference { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
            TypeResolutionError::UnknownTraitName { source_ref, .. } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
            TypeResolutionError::BoundedTraitReference {
                source_ref,
                target_ref,
            } => {
                let (hir_module, _) = hir::lower_file(db, source_ref.module_id);
                hir_module.get_type_reference_range(source_ref.local_id)
            }
        }
    }
}
