use text_size::TextRange;

use alloy_lexer::{Token, TokenKind};

pub(crate) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    cursor: usize,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(crate) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(crate) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.eat_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    pub(crate) fn peek_nth_kind(&mut self, skip: usize) -> Option<TokenKind> {
        self.peek_kind_raw(skip)
    }

    pub(crate) fn peek_nth_token(&mut self, skip: usize) -> Option<&Token> {
        self.peek_token_raw(skip)
    }

    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    fn peek_kind_raw(&mut self, skip: usize) -> Option<TokenKind> {
        self.peek_token_raw(skip).map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&mut self, skip: usize) -> Option<&Token> {
        self.eat_trivia();

        let mut cursor = self.cursor;
        let mut non_trivia_tokens_found = 0;

        while cursor < self.tokens.len() {
            let token = self.tokens.get(cursor)?;
            cursor += 1;
            if token.kind.is_trivia() {
                continue;
            }

            if non_trivia_tokens_found == skip {
                return Some(token);
            }

            non_trivia_tokens_found += 1;

        }
        None
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&mut self) -> bool {
        self.tokens
            .get(self.cursor)
            .map(|Token { kind, .. }| *kind)
            .map_or(false, TokenKind::is_trivia)
    }
}

#[cfg(test)]
mod tests {
    use alloy_lexer::Lexer;
    use super::*;

    #[test]
    fn peek_nth_kind_0_empty() {
        let input = "";
        let tokens: Vec<_> = Lexer::new(input).collect();
        let mut source = Source::new(&tokens);

        assert_eq!(
            source.peek_nth_kind(0),
            None
        )
    }

    #[test]
    fn peek_nth_kind_1_empty() {
        let input = "";
        let tokens: Vec<_> = Lexer::new(input).collect();
        let mut source = Source::new(&tokens);

        assert_eq!(
            source.peek_nth_kind(1),
            None
        )
    }

    #[test]
    fn peek_nth_kind_0_multiple() {
        let input = r#"
        typeof hi : String -> String
        "#;
        let tokens: Vec<_> = Lexer::new(input).collect();
        let mut source = Source::new(&tokens);

        assert_eq!(
            source.peek_nth_kind(0),
            Some(TokenKind::TypeOfKw)
        )
    }

    #[test]
    fn peek_nth_kind_1_multiple() {
        let input = r#"
        typeof hi : String -> String
        "#;
        let tokens: Vec<_> = Lexer::new(input).collect();
        let mut source = Source::new(&tokens);

        assert_eq!(
            source.peek_nth_kind(1),
            Some(TokenKind::Ident)
        )
    }
}