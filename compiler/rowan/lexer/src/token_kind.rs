use std::fmt;

use logos::Logos;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Logos)]
#[logos(
    subpattern op_id = r"[!|<>=]+",
    subpattern upper_id = r"[A-Z]([a-zA-Z0-9_]*)",
    subpattern lower_id = r"_?[a-z]([a-zA-Z0-9_]*)",
    subpattern either_id = r"[a-zA-Z]([a-zA-Z0-9_]*)",
)]
pub enum TokenKind {
    #[regex(r"[ \t\f\r\n]+")]
    Whitespace,

    #[token("fn")]
    FnKw,

    #[token("let")]
    LetKw,

    #[token("if")]
    IfKw,
    #[token("then")]
    ThenKw,
    #[token("else")]
    ElseKw,

    #[regex("[A-Za-z][A-Za-z0-9]*")]
    Ident,

    #[regex("[0-9]+")]
    Integer,

    #[regex(r"[0-9]+\.[0-9]+")]
    Fractional,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    String,

    #[regex(r#"'([^'\\]|\\t|\\u|\\n|\\")*'"#)]
    Char,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("=")]
    Equals,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex("--[^\n]*")]
    Comment,

    #[error]
    Error,
}

impl TokenKind {
    #[must_use]
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Whitespace => "whitespace",
            Self::FnKw => "‘fn’",
            Self::LetKw => "‘let’",
            Self::IfKw => "‘if‘",
            Self::ThenKw => "‘then‘",
            Self::ElseKw => "‘else‘",
            Self::Ident => "identifier",
            Self::Integer => "integer",
            Self::Fractional => "fractional",
            Self::String => "string",
            Self::Char => "char",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Equals => "‘=’",
            Self::LParen => "‘(’",
            Self::RParen => "‘)’",
            Self::LBrace => "‘{’",
            Self::RBrace => "‘}’",
            Self::Comment => "comment",
            Self::Error => "an unrecognized token",
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Lexer;

    use super::*;

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(input);

        let token = lexer.next().unwrap();
        assert_eq!(token.kind, kind);
        assert_eq!(token.text, input);
    }

    #[test]
    fn lex_spaces_and_tabs_and_newlines() {
        check("  \t  \r \n ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_spaces() {
        check("   ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check("fn", TokenKind::FnKw);
    }

    #[test]
    fn lex_let_keyword() {
        check("let", TokenKind::LetKw);
    }

    #[test]
    fn lex_if_keyword() {
        check("if", TokenKind::IfKw);
    }

    #[test]
    fn lex_then_keyword() {
        check("then", TokenKind::ThenKw);
    }

    #[test]
    fn lex_else_keyword() {
        check("else", TokenKind::ElseKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check("abcd", TokenKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check("ab123cde456", TokenKind::Ident);
    }

    #[test]
    fn lex_single_char_identifier() {
        check("x", TokenKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check("ABCdef", TokenKind::Ident);
    }

    #[test]
    fn lex_integral() {
        check("123456", TokenKind::Integer);
    }

    #[test]
    fn lex_fractional() {
        check("123456.123456", TokenKind::Fractional);
    }

    #[test]
    fn lex_string() {
        check(r#""hello""#, TokenKind::String);
    }

    #[test]
    fn lex_char() {
        check("'char'", TokenKind::Char);
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", TokenKind::Minus);
    }

    #[test]
    fn lex_star() {
        check("*", TokenKind::Star);
    }

    #[test]
    fn lex_slash() {
        check("/", TokenKind::Slash);
    }

    #[test]
    fn lex_equals() {
        check("=", TokenKind::Equals);
    }

    #[test]
    fn lex_left_brace() {
        check("{", TokenKind::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check("}", TokenKind::RBrace);
    }

    #[test]
    fn lex_left_parenthesis() {
        check("(", TokenKind::LParen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check(")", TokenKind::RParen);
    }

    #[test]
    fn lex_comment() {
        check("-- foo", TokenKind::Comment);
    }
}
