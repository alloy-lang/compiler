#[allow(clippy::wildcard_imports)]
use super::*;

pub struct Path(SyntaxNode);

impl Path {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        Some(Self(node))
    }

    pub fn segments(&self) -> impl Iterator<Item = String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|node| matches!(node.kind(), SyntaxKind::Ident | SyntaxKind::OpIdent))
            .map(|token| token.text().to_string())
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Path")
            .field("segments", &self.segments().collect::<Vec<_>>())
            .finish()
    }
}
