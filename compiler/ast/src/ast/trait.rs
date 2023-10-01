#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TraitDef, fields: [name, members]);

impl TraitDef {
    #[must_use]
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    #[must_use]
    pub fn members(&self) -> Vec<TraitMember> {
        children(self)
    }
}

ast_union_node!(TraitMember, kinds: [SelfTypeVariable, NamedTypeVariable, TypeAnnotation, ValueDef]);
