use super::{Token, TokenKind};
use alloy_lexer::Token as LexerToken;
use alloy_lexer::TokenKind as LTK;

pub struct TokenConverter<'a, I: Iterator<Item = LexerToken<'a>>> {
    iter: I,
}

impl<'a, I: Iterator<Item = LexerToken<'a>>> TokenConverter<'a, I> {
    pub fn new(iter: I) -> TokenConverter<'a, I> {
        TokenConverter { iter }
    }

    pub fn from_lexer_token(&mut self, lexer_token: LexerToken<'a>) -> Token<'a> {
        let LexerToken { kind, mut span } = lexer_token;

        let new_kind = match kind {
            LTK::Error => TokenKind::Error,
            LTK::Whitespace => TokenKind::Whitespace,
            LTK::EOL => TokenKind::EOL,
            LTK::Comment(val) => TokenKind::Comment(val),
            LTK::DocComment(val) => TokenKind::DocComment(val),
            LTK::AttributeOpen => TokenKind::AttributeOpen,
            LTK::Import => TokenKind::Import,
            LTK::Module => TokenKind::Module,
            LTK::Where => TokenKind::Where,
            LTK::When => TokenKind::When,
            LTK::Match => TokenKind::Match,
            LTK::Trait => TokenKind::Trait,
            LTK::Behavior => TokenKind::Behavior,
            LTK::Typedef => TokenKind::Typedef,
            LTK::Typevar => TokenKind::Typevar,
            LTK::If => TokenKind::If,
            LTK::Then => TokenKind::Then,
            LTK::Else => TokenKind::Else,
            LTK::RightArrow => TokenKind::RightArrow,
            LTK::Comma => TokenKind::Comma,
            LTK::Dot => TokenKind::Dot,
            LTK::Colon => TokenKind::Colon,
            LTK::Eq => TokenKind::Eq,
            LTK::Lt => TokenKind::Lt,
            LTK::Gt => TokenKind::Gt,
            LTK::Minus => TokenKind::Minus,
            LTK::And => TokenKind::And,
            LTK::Or => TokenKind::Or,
            LTK::Plus => TokenKind::Plus,
            LTK::Pipe => TokenKind::Pipe,
            LTK::InvalidUpperIdentifier(val) => TokenKind::InvalidUpperIdentifier(val),
            LTK::LowerIdentifier(val) => TokenKind::LowerIdentifier(val),
            LTK::UpperIdentifier(val) => TokenKind::UpperIdentifier(val),
            LTK::OperatorIdentifier(val) => TokenKind::OperatorIdentifier(val),
            LTK::NilIdentifier => TokenKind::NilIdentifier,
            LTK::InvalidUpperPath(val) => TokenKind::InvalidUpperPath(val),
            LTK::UpperPath(val) => TokenKind::UpperPath(val),
            LTK::LowerPath(val) => TokenKind::LowerPath(val),
            LTK::LiteralString(val) => TokenKind::LiteralString(val),
            LTK::LiteralInt(val) => TokenKind::LiteralInt(val),
            LTK::LiteralFloat(val) => TokenKind::LiteralFloat(val),
            LTK::EOF => TokenKind::EOF,

            LTK::OpenParen => {
                let mut tokens = Vec::new();
                while let Some(inner) = self.next() {
                    span = span.start..inner.span.end;

                    if TokenKind::CloseParen == inner.kind {
                        break;
                    }

                    tokens.push(Token {
                        kind: inner.kind,
                        span: inner.span,
                    });
                }

                TokenKind::Parens(tokens)
            }
            LTK::CloseParen => TokenKind::CloseParen,
            LTK::OpenBrace => {
                let mut tokens = Vec::new();
                while let Some(inner) = self.next() {
                    span = span.start..inner.span.end;

                    if TokenKind::CloseBrace == inner.kind {
                        break;
                    }

                    tokens.push(Token {
                        kind: inner.kind,
                        span: inner.span,
                    });
                }

                TokenKind::Braces(tokens)
            }
            LTK::CloseBrace => TokenKind::CloseBrace,
            LTK::OpenSqBracket => {
                let mut tokens = Vec::new();
                while let Some(inner) = self.next() {
                    span = span.start..inner.span.end;

                    if TokenKind::CloseSqBracket == inner.kind {
                        break;
                    }

                    tokens.push(Token {
                        kind: inner.kind,
                        span: inner.span,
                    });
                }

                TokenKind::SqBrackets(tokens)
            }
            LTK::CloseSqBracket => TokenKind::CloseSqBracket,
        };

        Token {
            kind: new_kind,
            span,
        }
    }
}

impl<'a, I: Iterator<Item = LexerToken<'a>>> Iterator for TokenConverter<'a, I> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(t) => Some(self.from_lexer_token(t)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_lexer::TokenStream as LTS;
    use pretty_assertions::assert_eq;

    //
    // Parens ()
    //

    #[test]
    fn test_single_parens() {
        let source = "()";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Parens(vec![]),
            span: 0..2,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_single_parens() {
        let source = "(())";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Parens(vec![Token {
                kind: TokenKind::Parens(vec![]),
                span: 1..3,
            }]),
            span: 0..4,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_tuple_parens() {
        let source = "( (Int), (String) )";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Parens(vec![
                Token {
                    kind: TokenKind::Parens(vec![Token {
                        kind: TokenKind::UpperIdentifier("Int"),
                        span: 3..6,
                    }]),
                    span: 2..7,
                },
                Token {
                    kind: TokenKind::Comma,
                    span: 7..8,
                },
                Token {
                    kind: TokenKind::Parens(vec![Token {
                        kind: TokenKind::UpperIdentifier("String"),
                        span: 10..16,
                    }]),
                    span: 9..17,
                },
            ]),
            span: 0..19,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_open_paren() {
        let source = "(";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Parens(vec![]),
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_close_paren() {
        let source = ")";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::CloseParen,
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }

    //
    // Braces {}
    //

    #[test]
    fn test_single_braces() {
        let source = "{}";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Braces(vec![]),
            span: 0..2,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_single_braces() {
        let source = "{{}}";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Braces(vec![Token {
                kind: TokenKind::Braces(vec![]),
                span: 1..3,
            }]),
            span: 0..4,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_tuple_braces() {
        let source = "{ {Int}, {String} }";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Braces(vec![
                Token {
                    kind: TokenKind::Braces(vec![Token {
                        kind: TokenKind::UpperIdentifier("Int"),
                        span: 3..6,
                    }]),
                    span: 2..7,
                },
                Token {
                    kind: TokenKind::Comma,
                    span: 7..8,
                },
                Token {
                    kind: TokenKind::Braces(vec![Token {
                        kind: TokenKind::UpperIdentifier("String"),
                        span: 10..16,
                    }]),
                    span: 9..17,
                },
            ]),
            span: 0..19,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_open_brace() {
        let source = "{";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::Braces(vec![]),
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_close_brace() {
        let source = "}";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::CloseBrace,
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }

    //
    // Square Brackets {}
    //

    #[test]
    fn test_single_sq_brackets() {
        let source = "[]";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::SqBrackets(vec![]),
            span: 0..2,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_single_sq_brackets() {
        let source = "[[]]";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::SqBrackets(vec![Token {
                kind: TokenKind::SqBrackets(vec![]),
                span: 1..3,
            }]),
            span: 0..4,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_tuple_sq_brackets() {
        let source = "[ [Int], [String] ]";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::SqBrackets(vec![
                Token {
                    kind: TokenKind::SqBrackets(vec![Token {
                        kind: TokenKind::UpperIdentifier("Int"),
                        span: 3..6,
                    }]),
                    span: 2..7,
                },
                Token {
                    kind: TokenKind::Comma,
                    span: 7..8,
                },
                Token {
                    kind: TokenKind::SqBrackets(vec![Token {
                        kind: TokenKind::UpperIdentifier("String"),
                        span: 10..16,
                    }]),
                    span: 9..17,
                },
            ]),
            span: 0..19,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_open_sq_bracket() {
        let source = "[";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::SqBrackets(vec![]),
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_floating_close_sq_bracket() {
        let source = "]";
        let tokens = LTS::from_source(source);
        let actual = TokenConverter::new(tokens).collect::<Vec<_>>();

        let expected = vec![Token {
            kind: TokenKind::CloseSqBracket,
            span: 0..1,
        }];
        assert_eq!(expected, actual);
    }
}
