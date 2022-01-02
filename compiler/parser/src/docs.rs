use alloy_lexer::{Token, TokenKind, TokenStream};
use itertools::Itertools;

#[must_use]
pub fn extract_doc_comments<'a>(stream: &'a mut TokenStream<'a>) -> Vec<Token<'a>> {
    stream
        .take_while_ref(|t| {
            matches!(
                t.kind,
                TokenKind::EOL | TokenKind::Comment(_) | TokenKind::DocComment(_)
            )
        })
        .filter(|t| matches!(t.kind, TokenKind::DocComment(_)))
        .collect::<Vec<_>>()
}
