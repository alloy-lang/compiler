#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(BehaviorDef, fields: [trait_, type_, members]);

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
    pub fn members(&self) -> Vec<BehaviorMember> {
        children(self)
    }
}

ast_union_node!(BehaviorMember, kinds: [NamedTypeVariable, TypeAnnotation, ValueDef]);
