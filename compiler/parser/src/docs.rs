use alloy_lexer::{TokenKind, TokenStream};
use itertools::Itertools;

#[must_use]
pub fn extract_doc_comments<'a>(stream: &'a mut TokenStream<'a>) -> Vec<String> {
    stream
        .take_while_ref(|t| {
            matches!(
                t.kind,
                TokenKind::EOL | TokenKind::Comment(_) | TokenKind::DocComment(_)
            )
        })
        .filter_map(|t| match t.kind {
            TokenKind::DocComment(s) => Some(s.to_string()),
            _ => None,
        })
        .collect::<Vec<_>>()
}
