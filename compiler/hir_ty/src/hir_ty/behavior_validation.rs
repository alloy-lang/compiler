//! Validation for trait implementations in behaviors

use crate::diagnostics::TypeInferenceErrorKind;
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_workspace::ModuleId;

/// Validate that all behaviors implement all abstract members from their implemented traits
pub(super) fn validate_behaviors(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Iterate through all behaviors in the module
    for (behavior_idx, _behavior, range, _name_op) in hir_module.behaviors() {
        let behavior = res::resolve_behavior_by_id(db, module_id, behavior_idx);
        validate_behavior(db, &behavior, range, result);
    }
}

fn validate_behavior(
    db: &dyn HirTyDatabase,
    behavior: &res::Behavior,
    behavior_range: text_size::TextRange,
    result: &mut HirTypedModule,
) {
    let trait_fql = match &behavior.attached_trait {
        Ok(trait_fql) => trait_fql,
        Err(err) => {
            result.error(
                TypeInferenceErrorKind::TypeResolutionError(err.clone()),
                behavior_range,
            );
            return;
        }
    };
    let type_fql = match &behavior.attached_type {
        Ok(type_fql) => type_fql,
        Err(err) => {
            result.error(
                TypeInferenceErrorKind::TypeResolutionError(err.clone()),
                behavior_range,
            );
            return;
        }
    };

    // Get the trait definition
    let (trait_module, _) = hir::lower_file(db, trait_fql.module_id);
    let trait_def = trait_module.get_trait(trait_fql.local_id);

    // Check all abstract members of the trait
    for (member_name, _type_annotation) in trait_def.abstract_members() {
        // Check if behavior has an implementation for this member
        if !behavior.has_implementation(member_name) {
            result.error(
                TypeInferenceErrorKind::MissingTraitImplementation {
                    trait_name: trait_def.name().to_string(),
                    member_name: member_name.to_string(),
                    type_fql: type_fql.clone(),
                },
                behavior_range,
            );
        }
    }
}
