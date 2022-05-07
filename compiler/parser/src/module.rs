#[cfg(test)]
mod module_parser_tests {
    use pretty_assertions::assert_eq;

    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned};

    #[test]
    fn test_empty_source() {
        let source = "   \n  ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleDefinition {
            span: 0..6,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 6..6,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_identifier() {
        let source = "module ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 6..7,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 7..7,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_identifier() {
        let source = "module _ ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 7..8,
            actual: vec![
                Token {
                    kind: TokenKind::NilIdentifier,
                    span: 7..8,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 9..9,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_where() {
        let source = "module Test ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 11..12,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 12..12,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_where() {
        let source = "module Test when";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 12..16,
            actual: vec![
                Token {
                    kind: TokenKind::When,
                    span: 12..16,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 16..16,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unexpected_remainder() {
        {
            let source = "module Test where trait";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 23..23,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
        {
            // with comment
            let source = "module Test where trait -- stuff";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 32..32,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
        {
            // with doc comment
            let source = "module Test where trait --! extra stuff";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 39..39,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_empty_module() {
        let source = test_source::EMPTY_MODULE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_lowercase() {
        let source = "module test where";
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `test`".to_string(),
            span: 7..11,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_underscore() {
        let source = "module _Test where";
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `_Test`".to_string(),
            span: 7..12,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_orphaned_identifier() {
        let source = r#"
                module Test
                where

                thing
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::OrphanedIdentifier {
            span: 68..73,
            name: "thing".to_string(),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_partial_value_declaration_with_eq() {
        let source = r#"
            module Test
            where

            thing =
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedExpr {
            span: 56..63,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 72..72,
            }],
        });

        assert_eq!(expected, actual);
    }
}
