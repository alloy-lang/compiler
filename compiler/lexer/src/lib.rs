extern crate maplit;

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token<'a> {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,

    // #[regex(r"\n[ \t\n\f]+")]
    // Indent(&'a str),

    // Comments
    #[regex(r"--[^\n]*\n")]
    Comment(&'a str),
    #[regex(r"--![^\n]*\n")]
    DocComment(&'a str),

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
    Arrow,
    #[token("::")]
    Connector,
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
    #[token("?")]
    Question,
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
    #[regex("_?[a-zA-Z]+")]
    Identifier(&'a str),
    #[regex(r"\([|<>=]+\)")]
    #[regex(r"[|<>=]+")]
    OperatorIdentifier(&'a str),
    #[token("_")]
    NilIdentifier,

    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    LiteralString,
    #[regex(r#"-?[0-9]+"#, |lex| lex.slice().parse())]
    LiteralInt(i64),
    #[regex(r#"-?[0-9]+\.[0-9]+"#, |lex| lex.slice().parse())]
    LiteralFloat(f64),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;
    use maplit::hashmap;
    use std::fs;

    fn assert_lex<'a>(source: &'a str, expected: Token) {
        let mut lex = Token::lexer(source);

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

    fn assert_lexes<'a>(source: &'a str, expected: &[Token]) {
        let mut lex = Token::lexer(source);

        let mut actual = Vec::new();
        while let Some(token) = lex.next() {
            actual.push(token);
        }

        assert_eq!(expected, actual);
    }

    fn assert_no_errors(source: &str) {
        let mut lex = Token::lexer(source);

        while let Some(token) = lex.next() {
            assert_ne!(
                token,
                super::Token::Error,
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
            "import" => Token::Import,
            "module" => Token::Module,
            "where" => Token::Where,
            "when" => Token::When,
            "match" => Token::Match,
            "trait" => Token::Trait,
            "behavior" => Token::Behavior,
            "typedef" => Token::Typedef,
            "typevar" => Token::Typevar,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
        };

        for (source, expected) in source {
            assert_lex(source, expected)
        }
    }

    #[test]
    fn test_identifiers() {
        let source = hashmap! {
            "_" => Token::NilIdentifier,
            "asdf" => Token::Identifier("asdf"),
            "_asdf" => Token::Identifier("_asdf"),
            "(>>=)" => Token::OperatorIdentifier("(>>=)"),
            ">>=" => Token::OperatorIdentifier(">>="),
            "(<=<)" => Token::OperatorIdentifier("(<=<)"),
            "<=<" => Token::OperatorIdentifier("<=<"),
            "(|>)" => Token::OperatorIdentifier("(|>)"),
            "|>" => Token::OperatorIdentifier("|>"),
        };

        for (source, expected) in source {
            assert_lex(source, expected)
        }
    }

    #[test]
    fn test_attributes() {
        let source: HashMap<&str, Vec<Token>> = hashmap! {
            "#[]" => vec![
                Token::AttributeOpen,
                Token::CloseBracket,
            ],
            "#[test]" => vec![
                Token::AttributeOpen,
                Token::Identifier("test"),
                Token::CloseBracket,
            ],
            "#[cfg(test)]" => vec![
                Token::AttributeOpen,
                Token::Identifier("cfg"),
                Token::OpenParen,
                Token::Identifier("test"),
                Token::CloseParen,
                Token::CloseBracket,
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
                Token::Module,
                Token::Identifier("Test"),
                Token::Where,
                Token::Trait,
                Token::Identifier("MultiVar"),
                Token::Lt,
                Token::Identifier("a"),
                Token::Comma,
                Token::Identifier("b"),
                Token::Gt,
                Token::Where,
                Token::Typevar,
                Token::Identifier("a"),
                Token::Typevar,
                Token::Identifier("b"),
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
                Token::Module,
                Token::Identifier("Test"),
                Token::Where,
                Token::Identifier("convert"),
                Token::Colon,
                Token::Identifier("List"),
                Token::Lt,
                Token::Identifier("String"),
                Token::Gt,
                Token::Arrow,
                Token::Identifier("List"),
                Token::Lt,
                Token::Identifier("Int"),
                Token::Gt,
            ],
        );
    }
}
