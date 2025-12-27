use crate::type_definition::resolve_type_definition_by_ref_id;
use crate::type_variable::{resolve_type_variable_by_id, TypeVariable};
use crate::{resolve_trait_by_ref_id, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Iter;

#[derive(Debug, Clone, PartialEq)]
pub struct Behavior {
    pub attached_trait: Result<Fql<hir::Trait>, TypeResolutionError>,
    pub attached_type: Result<Fql<hir::TypeDefinition>, TypeResolutionError>,
    pub named_type_variables: FxHashMap<hir::Name, (TypeVariable, Vec<TypeResolutionError>)>,
    pub type_annotations: FxHashMap<hir::Name, Fql<hir::TypeReference>>,
    pub values: FxHashMap<hir::Name, Fql<hir::Expression>>,
}

impl Behavior {
    pub fn has_implementation(&self, name: &hir::Name) -> bool {
        self.values.contains_key(name)
    }

    pub fn named_type_variables(
        &'_ self,
    ) -> Iter<'_, hir::Name, (TypeVariable, Vec<TypeResolutionError>)> {
        self.named_type_variables.iter()
    }

    pub fn type_annotations(&'_ self) -> Iter<'_, hir::Name, Fql<hir::TypeReference>> {
        self.type_annotations.iter()
    }

    pub fn values(&'_ self) -> Iter<'_, hir::Name, Fql<hir::Expression>> {
        self.values.iter()
    }
}

#[salsa::tracked]
pub fn resolve_behavior_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    behavior_id: hir::BehaviorIdx,
) -> Behavior {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let behavior = hir_module.get_behavior(behavior_id);

    let attached_trait = resolve_trait_by_ref_id(db, module_id, behavior.attached_trait);
    let attached_type = resolve_type_definition_by_ref_id(db, module_id, behavior.attached_type);

    let named_type_variables = behavior
        .named_type_variables()
        .map(|(name, type_def_idx)| {
            let type_variable = resolve_type_variable_by_id(db, module_id, *type_def_idx);
            (name.clone(), type_variable)
        })
        .collect();

    let type_annotations = behavior
        .type_annotations()
        .map(|(name, type_ref_idx)| {
            let type_ref = Fql::new(module_id, *type_ref_idx);
            (name.clone(), type_ref)
        })
        .collect();

    let values = behavior
        .values()
        .map(|(name, expr_idx)| {
            let expr = Fql::new(module_id, *expr_idx);
            (name.clone(), expr)
        })
        .collect();

    Behavior {
        attached_trait,
        attached_type,
        named_type_variables,
        type_annotations,
        values,
    }
}
