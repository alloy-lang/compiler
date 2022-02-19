extern crate maplit;

use logos::{Lexer, Logos};
use non_empty_vec::{EmptyError, NonEmpty};
use std::convert::TryFrom;
use std::ops::Range;

#[derive(Clone)]
pub struct TokenStream<'source> {
    lexer: logos::Lexer<'source, TokenKind<'source>>,
}

impl<'source> TokenStream<'source> {
    #[must_use]
    pub fn from_source(source: &'source str) -> TokenStream<'source> {
        TokenStream {
            lexer: TokenKind::lexer(source),
        }
    }
}

impl<'source> Iterator for TokenStream<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer
            .next()
            .map(|token| Token::new(token, self.lexer.span()))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'source> {
    pub kind: TokenKind<'source>,
    pub span: Range<usize>,
}

impl<'source> Token<'source> {
    fn new(kind: TokenKind<'source>, span: Range<usize>) -> Self {
        Token { kind, span }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(
    subpattern op_id = r"[!|<>=]+",
    subpattern upper_id = r"[A-Z]([a-zA-Z0-9_]*)",
    subpattern lower_id = r"_?[a-z]([a-zA-Z0-9_]*)",
    subpattern either_id = r"[a-zA-Z]([a-zA-Z0-9_]*)",
)]
pub enum TokenKind<'source> {
    #[error]
    Error,

    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,
    #[regex(r"[\n]+")]
    EOL,

    // #[regex(r"\n[ \t\n\f]+")]
    // Indent(&'source str),

    // Comments
    #[regex(r"--[^\n]*\n?")]
    Comment(&'source str),
    #[regex(r"--![^\n]*\n?")]
    DocComment(&'source str),

    // Attributes
    #[token("#[")]
    AttributeOpen,

    // Keywords
    #[token("import")]
    Import,
    #[token("module")]
    Module,
    #[token("where")]
    Where,
    #[token("when")]
    When,
    #[token("match")]
    Match,
    #[token("trait")]
    Trait,
    #[token("behavior")]
    Behavior,
    #[token("typedef")]
    Typedef,
    #[token("typevar")]
    Typevar,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,

    // Non-alphanumeric
    #[token("->")]
    RightArrow,
    // #[token(";")]
    // Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    // #[token("@")]
    // At,
    // #[token("#")]
    // Pound,
    // #[token("~")]
    // Tilde,
    // #[token("?")]
    // Question,
    #[token(":")]
    Colon,
    // #[token("$")]
    // Dollar,
    #[token("=")]
    Eq,
    // #[token("!")]
    // Bang,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("-")]
    Minus,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("+")]
    Plus,
    #[token("|")]
    Pipe,
    // #[token("*")]
    // Star,
    // #[token("/")]
    // Slash,
    // #[token("^")]
    // Caret,
    // #[token("%")]
    // Percent,
    #[regex("#(?&upper_id)")]
    HashUpperIdentifier(&'source str),
    #[regex("_(?&upper_id)[?]?")]
    InvalidUpperIdentifier(&'source str),
    #[regex("(?&lower_id)[?]?")]
    LowerIdentifier(&'source str),
    #[regex("(?&upper_id)")]
    UpperIdentifier(&'source str),
    #[regex(r"\((?&op_id)\)")]
    #[regex(r"(?&op_id)")]
    OperatorIdentifier(&'source str),
    #[token("_")]
    NilIdentifier,

    #[regex("((?&either_id)::)+")]
    #[regex("((?&either_id)(:))+(?&upper_id)")]
    InvalidUpperPath(&'source str),
    #[regex("((?&either_id)::)+(?&upper_id)", extract_path)]
    UpperPath(NonEmpty<String>),
    #[regex("((?&either_id)::)+(?&lower_id)[?]?", extract_path)]
    #[regex("((?&either_id)::)+\\((?&op_id)\\)", extract_path)]
    LowerPath(NonEmpty<String>),

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    LiteralString(&'source str),
    #[regex(r#"-?[0-9]+"#, |lex| lex.slice().parse())]
    LiteralInt(i64),
    #[regex(r#"-?[0-9]+\.[0-9]+"#, |lex| lex.slice().parse())]
    LiteralFloat(f64),

    EOF,
}

fn extract_path<'source>(
    lex: &mut Lexer<'source, TokenKind<'source>>,
) -> Result<NonEmpty<String>, EmptyError> {
    let slice = lex.slice();
    let segments = slice
        .split("::")
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    NonEmpty::try_from(segments)
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use maplit::hashmap;
    use pretty_assertions::assert_eq;
    use std::fs;

    fn assert_lex(source: &str, expected: TokenKind) {
        let mut lex = TokenKind::lexer(source);

        if let Some(token) = lex.next() {
            assert_eq!(
                token,
                expected,
                r#"
                slice: {:?}
                span: '{:?}'
                source: {:?}
                "#,
                lex.slice(),
                lex.span(),
                source,
            );
        }

        assert_eq!(lex.next(), None);
    }

    fn assert_lexes<'a>(source: &'a str, expected: &[TokenKind]) {
        let actual = TokenStream::from_source(source)
            .map(|t| t.kind)
            .collect::<Vec<_>>();

        assert_eq!(expected, actual);
    }

    fn assert_no_errors(source: &str) {
        let mut lex = TokenKind::lexer(source);

        while let Some(token) = lex.next() {
            assert!(
                !matches!(token, super::TokenKind::Error),
                r#"
                slice: {:?}
                span: '{:?}'
                source: {:?}
                "#,
                lex.slice(),
                lex.span(),
                source,
            )
        }

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        let source = hashmap! {
            "import" => TokenKind::Import,
            "module" => TokenKind::Module,
            "where" => TokenKind::Where,
            "when" => TokenKind::When,
            "match" => TokenKind::Match,
            "trait" => TokenKind::Trait,
            "behavior" => TokenKind::Behavior,
            "typedef" => TokenKind::Typedef,
            "typevar" => TokenKind::Typevar,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
        };

        for (source, expected) in source {
            assert_lex(source, expected)
        }
    }

    #[test]
    fn test_identifiers() {
        let source = hashmap! {
            "_" => TokenKind::NilIdentifier,
            "asdf" => TokenKind::LowerIdentifier("asdf"),
            "_asdf" => TokenKind::LowerIdentifier("_asdf"),
            "Asdf" => TokenKind::UpperIdentifier("Asdf"),
            "_Asdf" => TokenKind::InvalidUpperIdentifier("_Asdf"),
            "(>>=)" => TokenKind::OperatorIdentifier("(>>=)"),
            ">>=" => TokenKind::OperatorIdentifier(">>="),
            "(<=<)" => TokenKind::OperatorIdentifier("(<=<)"),
            "<=<" => TokenKind::OperatorIdentifier("<=<"),
            "(|>)" => TokenKind::OperatorIdentifier("(|>)"),
            "|>" => TokenKind::OperatorIdentifier("|>"),
            "!" => TokenKind::OperatorIdentifier("!"),
            "(!)" => TokenKind::OperatorIdentifier("(!)"),
            "!=" => TokenKind::OperatorIdentifier("!="),
            "(!=)" => TokenKind::OperatorIdentifier("(!=)"),
        };

        for (source, expected) in source {
            assert_lex(source, expected)
        }
    }

    #[test]
    fn test_paths() {
        unsafe {
            let source = hashmap! {
                "Number::is_positive?" => TokenKind::LowerPath(NonEmpty::new_unchecked(vec![
                    "Number".to_string(),
                    "is_positive?".to_string(),
                ])),
                "std::Number::is_positive?" => TokenKind::LowerPath(NonEmpty::new_unchecked(vec![
                    "std".to_string(),
                    "Number".to_string(),
                    "is_positive?".to_string(),
                ])),
                "std::function::(|>)" => TokenKind::LowerPath(NonEmpty::new_unchecked(vec![
                    "std".to_string(),
                    "function".to_string(),
                    "(|>)".to_string(),
                ])),
                "std::Number::" => TokenKind::InvalidUpperPath("std::Number::"),
                "std::Number" => TokenKind::UpperPath(NonEmpty::new_unchecked(vec![
                    "std".to_string(),
                    "Number".to_string(),
                ])),
                "std:Number" => TokenKind::InvalidUpperPath("std:Number"),
                // "std.Number" => TokenKind::InvalidUpperPath("std.Number"),
                // "std/Number" => TokenKind::InvalidUpperPath("std/Number"),
            };

            for (source, expected) in source {
                assert_lex(source, expected)
            }
        }
    }

    #[test]
    fn test_attributes() {
        let source = hashmap! {
            "#[]" => vec![
                TokenKind::AttributeOpen,
                TokenKind::CloseBracket,
            ],
            "#[test]" => vec![
                TokenKind::AttributeOpen,
                TokenKind::LowerIdentifier("test"),
                TokenKind::CloseBracket,
            ],
            "#[cfg(test)]" => vec![
                TokenKind::AttributeOpen,
                TokenKind::LowerIdentifier("cfg"),
                TokenKind::OpenParen,
                TokenKind::LowerIdentifier("test"),
                TokenKind::CloseParen,
                TokenKind::CloseBracket,
            ],
        };

        for (source, expected) in source {
            assert_lexes(source, &expected)
        }
    }

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;

        assert_no_errors(source)
    }

    #[test]
    fn test_int_value_declaration_no_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_int_value_declaration_with_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_float_value_declaration_no_type() {
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_float_value_declaration_with_type() {
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_single_arg_function_declaration_with_type() {
        let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source: &str = test_source::MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let source: &str = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;

        assert_no_errors(source)
    }

    #[test]
    fn test_simple_if_then_else() {
        let source: &str = test_source::SIMPLE_IF_THEN_ELSE;

        assert_no_errors(source)
    }

    #[test]
    fn test_nested_if_then_else() {
        let source: &str = test_source::NESTED_IF_THEN_ELSE;

        assert_no_errors(source)
    }

    #[test]
    fn test_multi_property_union_type() {
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_1;

            assert_no_errors(source);
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_2;

            assert_no_errors(source);
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_3;

            assert_no_errors(source);
        }
    }

    #[test]
    fn test_trait_with_no_typevar() {
        let source: &str = r#"
                module Test
                where

                trait Simple where
                  simple: Int -> Int
    "#;

        assert_no_errors(source)
    }

    #[test]
    fn test_trait_with_multiple_typevars() {
        let source: &str = r#"
                module Test
                where

                trait MultiVar<a, b> where
                  typevar a
                  typevar b
    "#;

        assert_no_errors(source);
        assert_lexes(
            source,
            &[
                TokenKind::EOL,
                TokenKind::Module,
                TokenKind::UpperIdentifier("Test"),
                TokenKind::EOL,
                TokenKind::Where,
                TokenKind::EOL,
                TokenKind::Trait,
                TokenKind::UpperIdentifier("MultiVar"),
                TokenKind::Lt,
                TokenKind::LowerIdentifier("a"),
                TokenKind::Comma,
                TokenKind::LowerIdentifier("b"),
                TokenKind::Gt,
                TokenKind::Where,
                TokenKind::EOL,
                TokenKind::Typevar,
                TokenKind::LowerIdentifier("a"),
                TokenKind::EOL,
                TokenKind::Typevar,
                TokenKind::LowerIdentifier("b"),
                TokenKind::EOL,
            ],
        );
    }

    #[test]
    fn test_bound_types() {
        let source: &str = r#"
                module Test
                where

                convert : List<String> -> List<Int>
    "#;

        assert_no_errors(source);
        assert_lexes(
            source,
            &[
                TokenKind::EOL,
                TokenKind::Module,
                TokenKind::UpperIdentifier("Test"),
                TokenKind::EOL,
                TokenKind::Where,
                TokenKind::EOL,
                TokenKind::LowerIdentifier("convert"),
                TokenKind::Colon,
                TokenKind::UpperIdentifier("List"),
                TokenKind::Lt,
                TokenKind::UpperIdentifier("String"),
                TokenKind::Gt,
                TokenKind::RightArrow,
                TokenKind::UpperIdentifier("List"),
                TokenKind::Lt,
                TokenKind::UpperIdentifier("Int"),
                TokenKind::Gt,
                TokenKind::EOL,
            ],
        );
    }

    #[test]
    fn test_std_lib() {
        let std_lib = fs::read_dir("../../std")
            .expect("Something went wrong reading the std lib dir")
            .map(|res| {
                res.expect("Something went wrong reading the directory entry")
                    .path()
            })
            .collect::<Vec<_>>();

        for path in std_lib {
            let file_name = path.to_str().expect("Expected filename");

            let source = fs::read_to_string(file_name).expect(&format!(
                "Something went wrong reading the file '{:?}'",
                file_name
            ));

            assert_no_errors(&source);
        }
    }
}
