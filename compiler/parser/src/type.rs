use core::convert;
use std::iter;

use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, T};

use super::parens;
use super::{ParseError, ParseResult, Span, Spanned};

fn parse_vec<'a>(type_name_span: &Span, input: Vec<Token<'a>>) -> ParseResult<'a, ast::Type> {
    self::parse(type_name_span, input.into_iter())
}

pub fn parse<'a>(
    type_name_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, ast::Type> {
    let (first_type, remainder) = parse_single_type(type_name_span, input)?;

    let mut remainder = remainder.into_iter().peekable();

    let first_span = first_type.span();

    log::debug!("*parse_type* remainder: {:?}", &remainder);

    match remainder.peek() {
        Some(Token {
            kind,
            span: arrow_span,
        }) if kind == &T![->] => {
            let previous_span = first_span.start..arrow_span.end;

            let (next_type, next_remainder) =
                parse(&previous_span, remainder.skip(1)).map_err(|e| match e {
                    ParseError::ExpectedType { span, actual } => {
                        ParseError::ExpectedLambdaReturnType { span, actual }
                    }
                    _ => e,
                })?;

            let span = first_span.start..next_type.span_end();
            let lambda = ast::Type::lambda(first_type.value, next_type.value);
            Ok((
                Spanned {
                    span,
                    value: lambda,
                },
                next_remainder,
            ))
        }
        Some(Token { kind: T![<>], span }) => Err(ParseError::ExpectedBoundTypeConstraints {
            span: span.clone(),
            actual: remainder.collect(),
        }),
        Some(Token { kind, span }) if kind == &T![<] => {
            let mut furthest_span = span.clone();
            remainder.next();

            let mut binds = Vec::new();

            loop {
                let (next_type, next_remainder) =
                    parse(&furthest_span, remainder).map_err(|e| match e {
                        ParseError::ExpectedType { span, actual } => {
                            ParseError::ExpectedBoundTypeConstraint { span, actual }
                        }
                        _ => e,
                    })?;
                remainder = next_remainder.into_iter().peekable();

                binds.push(next_type);

                match remainder.peek() {
                    Some(Token { kind: T![,], span }) => {
                        furthest_span = span.clone();
                        remainder.next();
                    }
                    Some(Token { kind: T![>], span }) => {
                        furthest_span = span.clone();
                        remainder.next();
                        break;
                    }
                    Some(Token { kind: _, span }) => {
                        return Err(ParseError::ExpectedBoundTypeComma {
                            span: furthest_span.end..span.start,
                            actual: remainder.collect(),
                        });
                    }
                    _ => {
                        return Err(ParseError::ExpectedBoundTypeComma {
                            span: furthest_span,
                            actual: remainder.collect(),
                        });
                    }
                }
            }

            let total_span = first_span.start..furthest_span.end;
            let bound = ast::Type::bound(
                first_type.clone().value,
                binds.into_iter().map(|s| s.value).collect(),
            );
            Ok((
                Spanned {
                    span: total_span,
                    value: bound,
                },
                remainder.collect(),
            ))
        }
        _ => Ok((first_type, remainder.collect())),
    }
}

fn parse_single_type<'a>(
    type_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, ast::Type> {
    let input = input.collect::<Vec<_>>();
    log::debug!("*parse_single_type* input: {:?}", &input);

    match_vec!(input.clone();
        [
            Token { kind: TokenKind::UpperIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned { span, value: ast::Type::identifier(id) }, remainder.collect())),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            remainder @ ..
        ] => Ok((
            Spanned { span, value: ast::Type::variable(id) },
            remainder.collect(),
        )),

        [
            Token { kind: T!['('], span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, ast::Type::tuple),

        [
            Token { kind, span: other_span },
            remainder @ ..
        ] => Err(ParseError::ExpectedType {
            span: type_span.start..other_span.end,
            actual: iter::once(Token { kind, span: other_span })
                .chain(remainder)
                .collect(),
        }),

        [remainder @ ..] => Err(ParseError::ExpectedType {
            span: type_span.clone(),
            actual: remainder.collect(),
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF { input, remaining })
    .and_then(convert::identity)
}

#[cfg(test)]
mod type_parser_tests {
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned, TypeAnnotation, TypeConstraint, TypeVariable};

    #[test]
    fn test_incomplete_function_type_annotation() {
        let source = r#"
            module Test
            where

            incomplete : Int -> Int ->
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> =
            Err(ParseError::ExpectedLambdaReturnType {
                span: 76..91,
                actual: vec![Token {
                    kind: TokenKind::EOF,
                    span: 91..91,
                }],
            });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_lambda_type_annotation_with_non_type_return() {
        let source = r#"
            module Test
            where

            incomplete : Int -> Int -> 0
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> =
            Err(ParseError::ExpectedLambdaReturnType {
                span: 76..84,
                actual: vec![
                    Token {
                        span: 83..84,
                        kind: TokenKind::LiteralInt(0),
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 93..93,
                    },
                ],
            });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_parens_unit_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : ("#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedClosedParen {
            span: 74..75,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 75..75,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_parens_tuple_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : (Int, String
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedClosedParen {
            span: 74..95,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 95..95,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_comma_tuple_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : (Int String)
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedTupleComma {
            span: 74..78,
            actual: vec![
                Token {
                    kind: TokenKind::UpperIdentifier("String"),
                    span: 79..85,
                },
                Token {
                    kind: TokenKind::CloseParen,
                    span: 85..86,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 95..95,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_type_annotation() {
        let source = r#"
            module Test
            where

            unit_type : ()"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..70,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..65,
                            value: "unit_type".to_string(),
                        },
                        t: Spanned {
                            span: 68..70,
                            value: ast::Type::Unit,
                        },
                        type_variables: vec![],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parens_type_annotation() {
        let source = r#"
            module Test
            where

            single_parens_type : (Int)"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..82,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..74,
                            value: "single_parens_type".to_string(),
                        },
                        t: Spanned {
                            span: 77..82,
                            value: ast::Type::identifier("Int"),
                        },
                        type_variables: vec![],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_many_parens() {
        let source = r#"
            module Test
            where

            many_parens_type : ((((), (Int))))"#;
        let actual: Result<Spanned<Module>, ParseError> = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..90,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..72,
                            value: "many_parens_type".to_string(),
                        },
                        t: Spanned {
                            span: 75..90,
                            value: ast::Type::tuple(vec![
                                ast::Type::Unit,
                                ast::Type::identifier("Int"),
                            ]),
                        },
                        type_variables: vec![],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_multiple_free_typevars() {
        let source = r#"
            module Test
            where

            with_typevars : (a -> b) -> a -> b where
                typevar a
                typevar b
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
                type_annotations: vec![Spanned {
                    span: 56..90,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..69,
                            value: "with_typevars".to_string(),
                        },
                        t: Spanned {
                            span: 72..90,
                            value: ast::Type::lambda(
                                ast::Type::lambda(
                                    ast::Type::variable("a"),
                                    ast::Type::variable("b"),
                                ),
                                ast::Type::lambda(
                                    ast::Type::variable("a"),
                                    ast::Type::variable("b"),
                                ),
                            ),
                        },
                        type_variables: vec![
                            Spanned {
                                span: 113..122,
                                value: TypeVariable::new_free("a", 121..122),
                            },
                            Spanned {
                                span: 139..148,
                                value: TypeVariable::new_free("b", 147..148),
                            },
                        ],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_incorrect_constrained_type() {
        let source = r#"
            module Test
            where

            with_incorrect_bound_type : List<"thing">
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedBoundTypeConstraint {
            span: 88..96,
            actual: vec![
                Token {
                    kind: TokenKind::LiteralString("\"thing\""),
                    span: 89..96,
                },
                Token {
                    kind: TokenKind::Gt,
                    span: 96..97,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 106..106,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_no_comma_constrained_type() {
        let source = r#"
            module Test
            where

            with_no_comma_bound_type : Tuple<Int String>
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedBoundTypeComma {
            span: 89..93,
            actual: vec![
                Token {
                    kind: TokenKind::UpperIdentifier("String"),
                    span: 93..99,
                },
                Token {
                    kind: TokenKind::Gt,
                    span: 99..100,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 109..109,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_no_comma_constrained_type_eof() {
        let source = r#"
            module Test
            where

            with_no_comma_bound_type : Tuple<Int
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedBoundTypeComma {
            span: 89..101,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 101..101,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_empty_constrained_type() {
        let source = r#"
            module Test
            where

            with_incorrect_bound_type : List<>
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedBoundTypeConstraints {
            span: 88..90,
            actual: vec![
                Token {
                    kind: TokenKind::EmptyAngledBrackets,
                    span: 88..90,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 99..99,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_constrained_type() {
        let source = r#"
            module Test
            where

            with_bound_type : List<String>
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
                type_annotations: vec![Spanned {
                    span: 56..86,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..71,
                            value: "with_bound_type".to_string(),
                        },
                        t: Spanned {
                            span: 74..86,
                            value: ast::Type::bound(
                                ast::Type::identifier("List"),
                                vec![ast::Type::identifier("String")],
                            ),
                        },
                        type_variables: vec![],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_constrained_typevars() {
        let source = r#"
            module Test
            where

            with_typevars : a<b> where
                typevar a = #Type<_>
                typevar b
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
                type_annotations: vec![Spanned {
                    span: 56..76,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..69,
                            value: "with_typevars".to_string(),
                        },
                        t: Spanned {
                            span: 72..76,
                            value: ast::Type::bound(
                                ast::Type::variable("a"),
                                vec![ast::Type::variable("b")],
                            ),
                        },
                        type_variables: vec![
                            Spanned {
                                span: 99..119,
                                value: TypeVariable {
                                    id: Spanned {
                                        span: 107..108,
                                        value: "a".to_string(),
                                    },
                                    constraints: vec![Spanned {
                                        span: 111..119,
                                        value: TypeConstraint::new_kind(1),
                                    }],
                                },
                            },
                            Spanned {
                                span: 136..145,
                                value: TypeVariable::new_free("b", 144..145),
                            },
                        ],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![],
            },
        });

        assert_eq!(expected, actual);
    }
}
