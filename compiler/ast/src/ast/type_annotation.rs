#[allow(clippy::wildcard_imports)]
use super::*;

pub struct TypeAnnotation(SyntaxNode);

impl TypeAnnotation {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeAnnotation {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
            .map(|token| token.text().into())
    }

    #[must_use]
    pub fn t(&self) -> Option<Type> {
        self.0.children_with_tokens().find_map(Type::cast)
    }
}

impl fmt::Debug for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeAnnotation")
            .field("name", &self.name())
            .field("type", &self.t())
            .finish()
    }
}
