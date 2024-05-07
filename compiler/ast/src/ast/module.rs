#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ModuleDef, fields: [name, imports, traits, behaviors, type_definitions, type_annotations, values]);

impl ModuleDef {
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        first_ident(self)
    }
    #[must_use]
    pub fn imports(&self) -> Vec<ImportDef> {
        children(self)
    }
    #[must_use]
    pub fn traits(&self) -> Vec<TraitDef> {
        children(self)
    }
    #[must_use]
    pub fn behaviors(&self) -> Vec<BehaviorDef> {
        children(self)
    }
    #[must_use]
    pub fn type_definitions(&self) -> Vec<TypeDefinition> {
        children(self)
    }
    #[must_use]
    pub fn type_annotations(&self) -> Vec<TypeAnnotation> {
        children(self)
    }
    #[must_use]
    pub fn values(&self) -> Vec<ValueDef> {
        children(self)
    }
}
