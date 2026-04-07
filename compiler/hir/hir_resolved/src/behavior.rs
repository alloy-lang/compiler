use crate::type_definition::resolve_type_definition_by_ref_id;
use crate::type_variable::{resolve_type_variable_by_id, TypeVariable};
use crate::{resolve_trait_by_ref_id, Fql, HirResolutionError};
use alloy_hir_def as hir;
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedBehaviorMember {
    pub name: hir::Name,
    pub type_annotation: Option<Fql<hir::TypeReference>>,
    pub value: Fql<hir::Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Behavior {
    pub attached_trait: Result<Fql<hir::Trait>, HirResolutionError>,
    pub attached_type: Result<Fql<hir::TypeDefinition>, HirResolutionError>,
    pub named_type_variables: FxHashMap<hir::Name, (TypeVariable, Vec<HirResolutionError>)>,
    pub members: Vec<ResolvedBehaviorMember>,
}

impl Behavior {
    pub fn has_implementation(&self, name: &hir::Name) -> bool {
        self.members.iter().any(|member| member.name == *name)
    }
}

#[salsa::tracked]
pub fn resolve_behavior_by_id(
    db: &dyn hir::HirDefDatabase,
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

    let members = behavior
        .members()
        .iter()
        .map(|member| ResolvedBehaviorMember {
            name: member.name.clone(),
            type_annotation: member.type_annotation.map(|ta| Fql::new(module_id, ta)),
            value: Fql::new(module_id, member.value),
        })
        .collect();

    Behavior {
        attached_trait,
        attached_type,
        named_type_variables,
        members,
    }
}

#[salsa::tracked]
pub fn resolve_behaviors(
    db: &dyn hir::HirDefDatabase,
    module_id: ModuleId,
) -> FxHashMap<Fql<hir::TypeDefinition>, Vec<Behavior>> {
    let (hir_module, _) = hir::lower_file(db, module_id);

    let mut result: FxHashMap<_, Vec<_>> = FxHashMap::default();
    for (idx, _, _, _) in hir_module.behaviors() {
        let behavior = resolve_behavior_by_id(db, module_id, idx);
        if let Ok(attached_type) = &behavior.attached_type {
            result
                .entry(attached_type.clone())
                .or_default()
                .push(behavior);
        }
    }

    result
}
