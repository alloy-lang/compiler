#[allow(clippy::wildcard_imports)]
use super::*;

pub struct TypeDefinition(SyntaxNode);

impl TypeDefinition {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeDef {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn type_args(&self) -> impl Iterator<Item = String> {
        self.0
            .children()
            .filter(|node| node.kind() == SyntaxKind::BoundedTypeArg)
            .flat_map(|node| node.children_with_tokens())
            .filter(|node| node.kind() == SyntaxKind::Ident)
            .filter_map(SyntaxElement::into_token)
            .map(|token| token.text().to_string())
    }

    pub fn types(&self) -> impl Iterator<Item = TypeDefinitionMember> {
        self.0
            .children_with_tokens()
            .filter_map(|node| TypeDefinitionMember::cast(node.into_node()?))
    }
}

impl fmt::Debug for TypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeDefinition")
            .field("type_args", &self.type_args().collect::<Vec<_>>())
            .field("types", &self.types().collect::<Vec<_>>())
            .finish()
    }
}

pub struct TypeDefinitionMember(SyntaxNode);

impl TypeDefinitionMember {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeDefMember {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn name(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
            .map(|token| token.text().into())
    }

    pub fn properties(&self) -> impl Iterator<Item = Type> {
        self.0
            .children()
            .filter(|node| node.kind() == SyntaxKind::TypeDefMemberProperty)
            .flat_map(|node| node.children_with_tokens())
            .filter_map(Type::cast)
    }
}

impl fmt::Debug for TypeDefinitionMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeDefinitionMember")
            .field("name", &self.name())
            .field("properties", &self.properties().collect::<Vec<_>>())
            .finish()
    }
}
