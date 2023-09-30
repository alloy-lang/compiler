#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(BehaviorDef, fields: [trait_, type_, members]);

impl BehaviorDef {
    #[must_use]
    pub fn trait_(&self) -> Option<Type> {
        match_node(self, SyntaxKind::BehaviorTraitName)?
            .children_with_tokens()
            .find_map(Type::cast)
    }

    #[must_use]
    pub fn type_(&self) -> Option<Type> {
        match_node(self, SyntaxKind::BehaviorTypeName)?
            .children_with_tokens()
            .find_map(Type::cast)
    }

    pub fn members(&self) -> Vec<BehaviorMember> {
        children(self)
    }
}

ast_union_node!(BehaviorMember, kinds: [NamedTypeVariable, TypeAnnotation, ValueDef]);
