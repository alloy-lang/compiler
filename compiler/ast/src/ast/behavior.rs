#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(BehaviorDef, fields: [trait_, type_, named_type_variables, type_annotations, values]);

impl BehaviorDef {
    #[must_use]
    pub fn trait_(&self) -> Option<Type> {
        first_matching_child(self, SyntaxKind::BehaviorTraitName)
    }

    #[must_use]
    pub fn type_(&self) -> Option<Type> {
        first_matching_child(self, SyntaxKind::BehaviorTypeName)
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
