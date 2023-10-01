#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ModuleDef, fields: [definitions]);

impl ModuleDef {
    #[must_use]
    pub fn definitions(&self) -> Vec<ModuleDefinition> {
        children(self)
    }
}

ast_union_node!(ModuleDefinition, kinds: [
    ImportDef,
    TraitDef,
    BehaviorDef,
    TypeDefinition,
    TypeAnnotation,
    ValueDef
]);
