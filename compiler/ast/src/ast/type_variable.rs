#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(NamedTypeVariable, fields: [name, constraints]);

impl NamedTypeVariable {
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        first_ident(self)
    }

    #[must_use]
    pub fn constraints(&self) -> Vec<TypeVariableConstraint> {
        children(self)
    }
}

ast_node!(SelfTypeVariable, fields: [constraints]);

impl SelfTypeVariable {
    #[must_use]
    pub fn constraints(&self) -> Vec<TypeVariableConstraint> {
        children(self)
    }
}

ast_union_node!(TypeVariableConstraint, kinds: [TypeVariableKindConstraint, TypeVariableTraitConstraint]);

ast_node!(TypeVariableKindConstraint, fields: [arity]);

impl TypeVariableKindConstraint {
    #[must_use]
    pub fn arity(&self) -> usize {
        self.0
            .children_with_tokens()
            .filter(|node| node.kind() == SyntaxKind::NilIdentifier)
            .count()
    }
}

ast_node!(TypeVariableTraitConstraint, fields: [trait_]);

impl TypeVariableTraitConstraint {
    #[must_use]
    pub fn trait_(&self) -> Option<Path> {
        first_child(self)
    }
}
