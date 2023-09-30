#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TypeDefinition, fields: [type_args, types]);

impl TypeDefinition {
    pub fn type_args(&self) -> Vec<String> {
        match_nodes(self, SyntaxKind::BoundedTypeArg)
            .flat_map(|node| node.children_with_tokens())
            .filter_map(SyntaxElement::into_token)
            .filter(|node| node.kind() == SyntaxKind::Ident)
            .map(|token| token.text().into())
            .collect()
    }

    pub fn types(&self) -> Vec<TypeDefinitionMember> {
        children(self)
    }
}

ast_node!(TypeDefinitionMember, fields: [name, properties]);

impl TypeDefinitionMember {
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    pub fn properties(&self) -> Vec<Type> {
        match_nodes(self, SyntaxKind::TypeDefinitionMemberProperty)
            .flat_map(|node| node.children_with_tokens())
            .filter_map(Type::cast)
            .collect()
    }
}
