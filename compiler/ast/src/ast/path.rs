#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(Path, fields: [segments]);

impl Path {
    pub fn segments(&self) -> Vec<String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|node| matches!(node.kind(), SyntaxKind::Ident | SyntaxKind::OpIdent))
            .map(|token| token.text().to_string())
            .collect()
    }
}
