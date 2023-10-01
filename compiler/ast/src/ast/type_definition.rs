#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TypeDefinition, fields: [type_args, types]);

ast_token!(Ident, fields: [text]);

impl TypeDefinition {
    #[must_use]
    pub fn type_args(&self) -> Vec<String> {
        all_matching_children(self, SyntaxKind::BoundedTypeArg)
            .map(|token: Ident| token.text())
            .collect()
    }

    #[must_use]
    pub fn types(&self) -> Vec<TypeDefinitionMember> {
        children(self)
    }
}

ast_node!(TypeDefinitionMember, fields: [name, properties]);

impl TypeDefinitionMember {
    #[must_use]
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    #[must_use]
    pub fn properties(&self) -> Vec<Type> {
        all_matching_children(self, SyntaxKind::TypeDefinitionMemberProperty).collect()
    }
}
