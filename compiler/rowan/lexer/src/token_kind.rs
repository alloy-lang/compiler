use std::fmt;

use logos::Logos;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Logos)]
#[logos(
    subpattern op_id = r"([<>=]+[|<>=][<>=]+)|([|<>=][<>=]+)|([<>=]+[|<>=])|([<>=]+)",
    subpattern upper_id = r"[A-Z]([a-zA-Z0-9_]*)",
    subpattern lower_id = r"_?[a-z]([a-zA-Z0-9_]*)",
    subpattern either_id = r"[a-zA-Z]([a-zA-Z0-9_]*)",
)]
pub enum TokenKind {
    #[regex(r"[ \t\f\r\n]+")]
    Whitespace,

    // Keywords
    #[token("let")]
    LetKw,
    #[token("import")]
    ImportKw,
    #[token("module")]
    ModuleKw,
    #[token("where")]
    WhereKw,
    #[token("when")]
    WhenKw,
    #[token("match")]
    MatchKw,
    #[token("trait")]
    TraitKw,
    #[token("behavior")]
    BehaviorKw,
    #[token("typedef")]
    TypedefKw,
    #[token("typevar")]
    TypevarKw,
    #[token("typeof")]
    TypeOfKw,
    #[token("if")]
    IfKw,
    #[token("then")]
    ThenKw,
    #[token("else")]
    ElseKw,
    #[token("self")]
    SelfKw,
    #[token("end")]
    EndKw,
    #[token("Type")]
    TypeKw,

    #[regex("_?[A-Za-z][A-Za-z0-9]*")]
    #[regex(r"\((?&op_id)\)")]
    #[regex(r"(?&op_id)")]
    Ident,

    #[regex("[0-9]+")]
    Integer,

    #[regex(r"[0-9]+\.[0-9]+")]
    Fractional,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    String,

    #[regex(r#"'([^'\\]|\\t|\\u|\\n|\\")*'"#)]
    Char,

    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,

    #[token(",")]
    Comma,

    #[token("->")]
    RightArrow,

    #[token("#")]
    Hash,

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

    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token("<>")]
    ClosedAngle,

    #[token("_")]
    NilIdentifier,

    #[token("|")]
    Pipe,

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
            Self::LetKw => "‘let’",
            Self::ImportKw => "‘import‘",
            Self::ModuleKw => "‘module‘",
            Self::WhereKw => "‘where‘",
            Self::WhenKw => "‘when‘",
            Self::MatchKw => "‘match‘",
            Self::TraitKw => "‘trait‘",
            Self::BehaviorKw => "‘behavior‘",
            Self::TypedefKw => "‘typedef‘",
            Self::TypevarKw => "‘typevar‘",
            Self::TypeOfKw => "‘typeof‘",
            Self::IfKw => "‘if‘",
            Self::ThenKw => "‘then‘",
            Self::ElseKw => "‘else‘",
            Self::SelfKw => "‘self‘",
            Self::EndKw => "‘end‘",
            Self::TypeKw => "‘Type‘",
            Self::Ident => "identifier",
            Self::Integer => "integer",
            Self::Fractional => "fractional",
            Self::String => "string",
            Self::Char => "char",
            Self::Colon => "‘:’",
            Self::DoubleColon => "‘::’",
            Self::Comma => "‘,’",
            Self::RightArrow => "‘->’",
            Self::Hash => "‘#’",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Equals => "‘=’",
            Self::LParen => "‘(’",
            Self::RParen => "‘)’",
            Self::LBrace => "‘{’",
            Self::RBrace => "‘}’",
            Self::LAngle => "‘<’",
            Self::RAngle => "‘>’",
            Self::ClosedAngle => "‘<>’",
            Self::NilIdentifier => "‘_’",
            Self::Pipe => "‘|’",
            Self::Comment => "comment",
            Self::Error => "an unrecognized token",
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Lexer;
    use maplit::btreemap;
    use std::collections::BTreeMap;

    use super::*;

    #[track_caller]
    fn check(input: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(input);

        let token = lexer.next().unwrap();
        assert_eq!((token.text, token.kind), (input, kind));
    }

    #[track_caller]
    fn check_multiple(input: &str, kinds: &[TokenKind]) {
        let (actual_text, actual_kinds) = Lexer::new(input)
            .map(|token| (token.text.to_owned(), vec![token.kind]))
            .reduce(|(mut acc_text, mut acc_kinds), (text, kind)| {
                acc_text.push_str(&text);
                acc_kinds.push(kind[0]);

                (acc_text, acc_kinds)
            })
            .expect("lexer should have completed non-empty");

        assert_eq!(
            (input.to_owned(), kinds),
            (actual_text, actual_kinds.as_slice())
        );
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
    fn test_keywords() {
        let source = btreemap! {
            "import" => TokenKind::ImportKw,
            "module" => TokenKind::ModuleKw,
            "where" => TokenKind::WhereKw,
            "when" => TokenKind::WhenKw,
            "match" => TokenKind::MatchKw,
            "trait" => TokenKind::TraitKw,
            "behavior" => TokenKind::BehaviorKw,
            "typedef" => TokenKind::TypedefKw,
            "typevar" => TokenKind::TypevarKw,
            "typeof" => TokenKind::TypeOfKw,
            "if" => TokenKind::IfKw,
            "then" => TokenKind::ThenKw,
            "else" => TokenKind::ElseKw,
            "self" => TokenKind::SelfKw,
            "end" => TokenKind::EndKw,
        };

        for (source, expected) in source {
            check(source, expected)
        }
    }

    #[test]
    fn test_symbols() {
        let source = btreemap! {
            ":" => TokenKind::Colon,
            "::" => TokenKind::DoubleColon,
            "," => TokenKind::Comma,
            "->" => TokenKind::RightArrow,
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" => TokenKind::Star,
            "/" => TokenKind::Slash,
            "=" => TokenKind::Equals,
            "(" => TokenKind::LParen,
            ")" => TokenKind::RParen,
            "{" => TokenKind::LBrace,
            "}" => TokenKind::RBrace,
            "|" => TokenKind::Pipe,
            "<>" => TokenKind::ClosedAngle,
        };

        for (source, expected) in source {
            check(source, expected)
        }
    }

    #[test]
    fn test_non_ident_symbols() {
        let source: BTreeMap<&str, Vec<TokenKind>> = btreemap! {
            "||" => vec![TokenKind::Pipe, TokenKind::Pipe],
            "<::>" => vec![TokenKind::LAngle, TokenKind::DoubleColon, TokenKind::RAngle],
        };

        for (source, expected) in source {
            check_multiple(source, &expected)
        }
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
    fn operator_identifiers() {
        let source = btreemap! {
            "asdf" => TokenKind::Ident,
            "_asdf" => TokenKind::Ident,
            "Asdf" => TokenKind::Ident,
            "_Asdf" => TokenKind::Ident,
            "(>>=)" => TokenKind::Ident,
            ">>=" => TokenKind::Ident,
            "(<=<)" => TokenKind::Ident,
            "<=<" => TokenKind::Ident,
            "(|>)" => TokenKind::Ident,
            "|>" => TokenKind::Ident,
            "(<|)" => TokenKind::Ident,
            "<|" => TokenKind::Ident,
            "(<|>)" => TokenKind::Ident,
            "<|>" => TokenKind::Ident,
            "(<=>)" => TokenKind::Ident,
            "<=>" => TokenKind::Ident,
        };

        for (source, expected) in source {
            check(source, expected)
        }
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
    fn lex_comment() {
        check("-- foo", TokenKind::Comment);
    }
}
