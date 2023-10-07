#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TraitDef, fields: [name, self_type_variables, named_type_variables, type_annotations, values]);

impl TraitDef {
    #[must_use]
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    #[must_use]
    pub fn self_type_variables(&self) -> Vec<SelfTypeVariable> {
        children(self)
    }

    #[must_use]
    pub fn named_type_variables(&self) -> Vec<NamedTypeVariable> {
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
