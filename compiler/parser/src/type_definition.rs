use non_empty_vec::NonEmpty;

use alloy_lexer::{Token, TokenKind};

use crate::NamedType;

use super::r#type;
use super::{ParseError, ParseResult, Span, Spanned};

pub fn parse<'a>(
    type_name_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, NonEmpty<Spanned<NamedType>>> {
    let input = input
        .skip_while(|t| matches!(t.kind, TokenKind::Pipe))
        .peekable();

    let (first_named_type, remainder) = parse_named_type(type_name_span, input)?;

    let mut types = NonEmpty::new(first_named_type);

    let mut remainder = remainder.into_iter().peekable();
    while let Some(Token {
        kind: TokenKind::Pipe,
        span: pipe_span,
    }) = remainder.peek().cloned()
    {
        let (next_named_type, next_remainder) = parse_named_type(&pipe_span, remainder.skip(1))?;
        types.push(next_named_type);
        remainder = next_remainder.into_iter().peekable();
    }

    let (first_type, _) = types.split_first();
    let (last_type, _) = types.split_last();
    let span = first_type.span.start..last_type.span.end;

    let spanned = Spanned { span, value: types };

    Ok((spanned, remainder.collect()))
}

fn parse_named_type<'a>(
    pipe_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, NamedType> {
    let mut input = input.peekable();

    if let Some(Token {
        kind: TokenKind::UpperIdentifier(id),
        span: id_span,
    }) = input.peek().cloned()
    {
        let type_result = r#type::parse(&id_span, input.skip(1));
        let (type_, remainder) = match type_result {
            Ok((type_, remainder)) => (Some(type_), remainder),
            Err(ParseError::ExpectedType { actual, .. }) => (None, actual),
            Err(e) => return Err(e),
        };

        let span = {
            let start = id_span.start;
            let end = type_.clone().map_or_else(|| id_span.end, Spanned::span_end);

            start..end
        };
        let spanned = Spanned {
            span,
            value: NamedType {
                name: Spanned::from_span(id_span, id.into()),
                t: type_,
            },
        };

        Ok((spanned, remainder))
    } else {
        Err(ParseError::ExpectedTypeName {
            span: pipe_span.clone(),
            actual: input.collect::<Vec<_>>(),
        })
    }
}

#[cfg(test)]
mod type_definition_parser_tests {
    use non_empty_vec::NonEmpty;
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, NamedType, ParseError, Span, Spanned, TypeDefinition};

    #[test]
    fn test_simple_typedef() {
        let source = r#"
            module Test
            where

            typedef Name = String
        "#;
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
                type_definitions: vec![Spanned {
                    span: 56..77,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..68,
                            value: "Name".to_string(),
                        },
                        types: Spanned {
                            span: 71..77,
                            value: unsafe {
                                NonEmpty::new_unchecked(vec![spanned_named_type_empty(
                                    71..77,
                                    "String",
                                )])
                            },
                        },
                    },
                }],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_typedef_2_union() {
        let source = r#"
            module Bool
            where

            typedef Bool =
              | False
              | True
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Bool".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![Spanned {
                    span: 56..113,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..68,
                            value: "Bool".to_string(),
                        },
                        types: Spanned {
                            span: 87..113,
                            value: unsafe {
                                NonEmpty::new_unchecked(vec![
                                    spanned_named_type_empty(87..92, "False"),
                                    spanned_named_type_empty(109..113, "True"),
                                ])
                            },
                        },
                    },
                }],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_typedef_3_union() {
        let source = r#"
            module Test
            where

            typedef Thing =
              | This
              | That
              | TheOther
        "#;
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
                type_definitions: vec![Spanned {
                    span: 56..138,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..69,
                            value: "Thing".to_string(),
                        },
                        types: Spanned {
                            span: 88..138,
                            value: unsafe {
                                NonEmpty::new_unchecked(vec![
                                    spanned_named_type_empty(88..92, "This"),
                                    spanned_named_type_empty(109..113, "That"),
                                    spanned_named_type_empty(130..138, "TheOther"),
                                ])
                            },
                        },
                    },
                }],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_typedef_with_properties() {
        let source = r#"
            module Test
            where

            typedef Shape = Circle (Float, Float, Float)
        "#;
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
                type_definitions: vec![Spanned {
                    span: 56..100,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..69,
                            value: "Shape".to_string(),
                        },
                        types: Spanned {
                            span: 72..100,
                            value: unsafe {
                                NonEmpty::new_unchecked(vec![spanned_named_type(
                                    72..78,
                                    "Circle",
                                    79..100,
                                    ast::Type::tuple(vec![
                                        ast::Type::identifier("Float"),
                                        ast::Type::identifier("Float"),
                                        ast::Type::identifier("Float"),
                                    ]),
                                )])
                            },
                        },
                    },
                }],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_union_typedef_with_properties() {
        let source = r#"
            module Test
            where

            typedef Shape =
              | Circle (Float, Float, Float)
              | Rectangle (Float, Float, Float, Float)
        "#;
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
                type_definitions: vec![Spanned {
                    span: 56..171,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..69,
                            value: "Shape".to_string(),
                        },
                        types: Spanned {
                            span: 88..171,
                            value: unsafe {
                                NonEmpty::new_unchecked(vec![
                                    spanned_named_type(
                                        88..94,
                                        "Circle",
                                        95..116,
                                        ast::Type::tuple(vec![
                                            ast::Type::identifier("Float"),
                                            ast::Type::identifier("Float"),
                                            ast::Type::identifier("Float"),
                                        ]),
                                    ),
                                    spanned_named_type(
                                        133..142,
                                        "Rectangle",
                                        143..171,
                                        ast::Type::tuple(vec![
                                            ast::Type::identifier("Float"),
                                            ast::Type::identifier("Float"),
                                            ast::Type::identifier("Float"),
                                            ast::Type::identifier("Float"),
                                        ]),
                                    ),
                                ])
                            },
                        },
                    },
                }],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_lowercase_union_name() {
        let source = r#"
            module Bool
            where

            typedef Bool =
              | True
              | false
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Err(ParseError::ExpectedTypeName {
            span: 106..107,
            actual: vec![
                Token {
                    kind: TokenKind::LowerIdentifier("false"),
                    span: 108..113,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 122..122,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_typedef_no_eq() {
        let source = r#"
            module Bool
            where

            typedef IncompleteUnion
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Err(ParseError::ExpectedEq {
            span: 64..79,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 88..88,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_typedef() {
        let source = r#"
            module Bool
            where

            typedef IncompleteUnion =
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Err(ParseError::ExpectedTypeName {
            span: 56..81,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 90..90,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_union() {
        let source = r#"
            module Bool
            where

            typedef IncompleteUnion = False |
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Err(ParseError::ExpectedTypeName {
            span: 88..89,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 98..98,
            }],
        });

        assert_eq!(expected, actual);
    }

    fn spanned_named_type_empty(span: Span, name: &str) -> Spanned<NamedType> {
        Spanned {
            span: span.clone(),
            value: NamedType {
                name: Spanned {
                    span: span.clone(),
                    value: name.to_string(),
                },
                t: None,
            },
        }
    }

    fn spanned_named_type(
        name_span: Span,
        name: &str,
        type_span: Span,
        t: ast::Type,
    ) -> Spanned<NamedType> {
        Spanned {
            span: name_span.start..type_span.end,
            value: NamedType {
                name: Spanned {
                    span: name_span,
                    value: name.to_string(),
                },
                t: Some(Spanned {
                    span: type_span,
                    value: t,
                }),
            },
        }
    }
}
