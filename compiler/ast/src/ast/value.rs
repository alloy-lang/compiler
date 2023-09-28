#[allow(clippy::wildcard_imports)]
use super::*;

pub struct ValueDefinition(SyntaxNode);

impl ValueDefinition {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::VariableDef {
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

    pub fn value(&self) -> Option<Expression> {
        self.0.children().find_map(Expression::cast)
    }
}

impl fmt::Debug for ValueDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueDefinition")
            .field("name", &self.name())
            .field("value", &self.value())
            .finish()
    }
}
