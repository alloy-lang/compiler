use core::convert;

use improved_slice_patterns::match_vec;
use itertools::Itertools;
use non_empty_vec::NonEmpty;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, T};

use super::{parens, pattern};
use super::{ParseError, ParseResult, Span, Spanned};

fn parse_vec<'a>(expr_span: &Span, input: Vec<Token<'a>>) -> ParseResult<'a, ast::Expr> {
    self::parse(expr_span, input.into_iter())
}

pub fn parse<'a>(
    expr_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, ast::Expr> {
    let input = input.collect::<Vec<_>>();
    log::debug!("*parse_expr* input: {:?}", &input);

    // let _doc_comments = extract_doc_comments(stream);

    match_vec!(input.clone();
        [
            Token { kind: T![|], span: start_pipe_span },
            remainder @ ..
        ] => {
            let mut remainder = remainder.clone();
            let mut pattern_remainder = remainder
                .take_while_ref(|t| !matches!(t.kind, T![|]))
                .collect::<Vec<_>>();

            let (pipe_span, expr, remainder) = match_vec!(remainder.collect::<Vec<_>>();
                [
                    Token { kind: T![|],  span: end_pipe_span },
                    Token { kind: T![->], span: arrow_span },
                    remainder @ ..
                ] => {
                    let pipe_span = start_pipe_span.start..end_pipe_span.end;
                    let (expr, remainder) = parse(&start_pipe_span, remainder)?;
                    Ok((pipe_span, expr, remainder))
                },

                [
                    Token { kind: T![|],  span: end_pipe_span },
                    remainder @ ..
                ] => {
                    let span = start_pipe_span.start..end_pipe_span.end;
                    Err(ParseError::ExpectedRightArrow {
                        span,
                        actual: pattern_remainder.clone(),
                    })
                },

                [remainder @ ..] => {
                    let span = expr_span.clone();
                    let span = span.start..pattern_remainder.get(0).map_or(span, Token::span).end;
                    Err(ParseError::ExpectedPipe {
                        span,
                        actual: pattern_remainder.clone(),
                    })
                }
            )
            .map_err(|remaining| ParseError::ExpectedEOF {
                input: vec![],
                remaining,
            })
            .and_then(convert::identity)?;

            let mut args = Vec::new();
            while !pattern_remainder.is_empty() {
                pattern_remainder = {
                    let (pattern_arg, remainder) = pattern::parse(&start_pipe_span, pattern_remainder)?;
                    args.push(pattern_arg.value);

                    match_vec!(remainder;
                        [
                            Token { kind: T![,], span },
                            remainder @ ..
                        ] => remainder.collect(),

                        [] => Vec::new()
                    )
                    .map_err(|remaining| ParseError::ExpectedLambdaArgsComma {
                        span: pipe_span.clone(),
                        actual: remaining,
                    })?
                };
            }

            Ok((Spanned {
                span: start_pipe_span.start..expr.span.end,
                value: ast::Expr::lambda(
                    args,
                    expr.value,
                ),
            }, remainder))
        },

        [
            Token { kind: TokenKind::LiteralInt(i), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::int_literal(i),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LiteralFloat(f), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::float_literal(f),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            Token { kind: T!['('],                        span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, |args| ast::Expr::application(NonEmpty::new(id), args)),

        [
            Token { kind: TokenKind::LowerPath(path), span },
            Token { kind: T!['('],                    span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, |args| ast::Expr::application(path, args)),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::identifier(id),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::UpperIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::identifier(id),
        }, remainder.collect())),

        [
            Token { kind: T!['('], span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, ast::Expr::tuple),

        [
            Token { kind: T![if], span: if_span },
            remainder @ ..
        ] => {
            let (if_expr, remainder) = self::parse(&if_span, remainder)?;
            let mut remainder = remainder.into_iter().peekable();

            match remainder.peek().cloned() {
                Some(t) if t.kind == T![then] => {
                    let (then_expr, remainder) = self::parse(&t.span, remainder.skip(1))?;
                    let mut remainder = remainder.into_iter().peekable();

                    match remainder.peek().cloned() {
                        Some(t) if t.kind == T![else] => {
                            let (else_expr, remainder) = self::parse(&t.span, remainder.skip(1))?;

                            Ok((Spanned {
                                span: if_span.start..else_expr.span.end,
                                value: ast::Expr::if_then_else(if_expr.value, then_expr.value, else_expr.value),
                            }, remainder))
                        }
                        Some(t) => Err(ParseError::ExpectedElseKeyWord {
                            span: t.span,
                            actual: remainder.collect_vec(),
                        }),
                        None => Err(ParseError::ExpectedElseKeyWord {
                            span: then_expr.span,
                            actual: vec![],
                        }),
                    }
                }
                Some(t) => Err(ParseError::ExpectedThenKeyWord {
                    span: t.span,
                    actual: remainder.collect_vec(),
                }),
                None => Err(ParseError::ExpectedThenKeyWord {
                    span: if_expr.span,
                    actual: vec![],
                }),
            }
        },

        [remainder @ ..,] => Err(ParseError::ExpectedExpr {
            span: expr_span.clone(),
            actual: remainder.collect(),
        }),
    )
        .map_err(|remaining: Vec<Token<'a>>| ParseError::ExpectedEOF {
            input,
            remaining,
        })
        .and_then(convert::identity)
        .and_then(|(expr1, remainder)| match_vec!(remainder;
            [
                Token { kind: T![+], span: op_span },
                remainder @ ..
            ] => {
                let expr_span = expr_span.start..op_span.end;

                let (expr2, remainder) = parse(&expr_span, remainder)?;

                Ok((Spanned {
                    span: expr_span.start..expr2.span.end,
                    value: ast::Expr::bin_op("+", expr1.value.clone(), expr2.value),
                }, remainder))
            },
            [
                Token { kind: T![-], span: op_span },
                remainder @ ..
            ] => {
                let expr_span = expr_span.start..op_span.end;

                let (expr2, remainder) = parse(&expr_span, remainder)?;

                Ok((Spanned {
                    span: expr_span.start..expr2.span.end,
                    value: ast::Expr::bin_op("-", expr1.value.clone(), expr2.value),
                }, remainder))
            },

            [remainder @ ..] => Ok(
                (expr1.clone(), remainder.collect())
            ),
        )
            .and_then(convert::identity)
            .or_else(|remainder| Ok((expr1, remainder)))
        )
}

#[cfg(test)]
mod expr_parser_tests {
    use non_empty_vec::NonEmpty;
    use ordered_float::NotNan;
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned, TypeAnnotation, Value};

    #[test]
    fn test_int_value_declaration_no_type() {
        let source = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
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
                values: vec![Spanned {
                    span: 56..65,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 64..65,
                            value: ast::Expr::int_literal(0),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    //noinspection DuplicatedCode
    #[test]
    fn test_int_value_declaration_with_type() {
        let source = test_source::INT_VALUE_DECLARATION_WITH_TYPE;
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
                    span: 56..67,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        t: Spanned {
                            span: 64..67,
                            value: ast::Type::identifier("Int"),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 80..89,
                    value: Value {
                        name: Spanned {
                            span: 80..85,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 88..89,
                            value: ast::Expr::Literal(ast::LiteralData::Integral(0)),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_float_value_declaration_no_type() {
        let source = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;
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
                values: vec![Spanned {
                    span: 56..67,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 64..67,
                            value: ast::Expr::Literal(ast::LiteralData::Fractional(
                                NotNan::new(0.1).unwrap(),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_float_value_declaration_with_type() {
        let source = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;
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
                    span: 56..69,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        t: Spanned {
                            span: 64..69,
                            value: ast::Type::identifier("Float"),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 82..93,
                    value: Value {
                        name: Spanned {
                            span: 82..87,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 90..93,
                            value: ast::Expr::Literal(ast::LiteralData::Fractional(
                                NotNan::new(0.1).unwrap(),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parens_expression() {
        let source = r#"
            module Test
            where

            parens = (1 + 2)
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
                values: vec![Spanned {
                    span: 56..72,
                    value: Value {
                        name: Spanned {
                            span: 56..62,
                            value: "parens".to_string(),
                        },
                        expr: Spanned {
                            span: 65..72,
                            value: ast::Expr::paren(ast::Expr::bin_op(
                                "+",
                                ast::Expr::int_literal(1),
                                ast::Expr::int_literal(2),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tuple_expression() {
        let source = r#"
            module Test
            where

            tuple = (1 + 2, 3)
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
                values: vec![Spanned {
                    span: 56..74,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "tuple".to_string(),
                        },
                        expr: Spanned {
                            span: 64..74,
                            value: ast::Expr::tuple(vec![
                                ast::Expr::bin_op(
                                    "+",
                                    ast::Expr::int_literal(1),
                                    ast::Expr::int_literal(2),
                                ),
                                ast::Expr::int_literal(3),
                            ]),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_pipe_lambda_expr() {
        let source = r#"
            module Test
            where

            no_right_arrow = |arg -> arg
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedPipe {
            span: 56..77,
            actual: vec![
                Token {
                    kind: TokenKind::LowerIdentifier("arg"),
                    span: 74..77,
                },
                Token {
                    kind: TokenKind::RightArrow,
                    span: 78..80,
                },
                Token {
                    kind: TokenKind::LowerIdentifier("arg"),
                    span: 81..84,
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
    fn test_no_right_arrow_lambda_expr() {
        let source = r#"
            module Test
            where

            no_right_arrow = |arg| arg
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedRightArrow {
            span: 73..78,
            actual: vec![Token {
                kind: TokenKind::LowerIdentifier("arg"),
                span: 74..77,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_body_lambda_expr() {
        let source = r#"
            module Test
            where

            no_body = |arg| ->
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedExpr {
            span: 66..67,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 83..83,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_comma_lambda_expr() {
        let source = r#"
            module Test
            where

            no_body = |arg1 arg2| -> arg1
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedLambdaArgsComma {
            span: 66..77,
            actual: vec![Token {
                kind: TokenKind::LowerIdentifier("arg2"),
                span: 72..76,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_single_arg_function_declaration_with_type() {
        let source = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 15..44,
            value: Module {
                name: Spanned {
                    span: 22..26,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 58..89,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 58..76,
                            value: "increment_positive".to_string(),
                        },
                        t: Spanned {
                            span: 79..89,
                            value: ast::Type::lambda(
                                ast::Type::identifier("Int"),
                                ast::Type::identifier("Int"),
                            ),
                        },
                    },
                }],
                values: vec![
                    Spanned {
                        span: 102..131,
                        value: Value {
                            name: Spanned {
                                span: 102..120,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 123..131,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::int_literal(0)],
                                    ast::Expr::int_literal(0),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 144..177,
                        value: Value {
                            name: Spanned {
                                span: 144..162,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 165..177,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("x")],
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("x"),
                                        ast::Expr::int_literal(1),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source = r#"


            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, "")| -> 0
            increment_by_length = |(x, y)| -> x + 1
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 15..44,
            value: Module {
                name: Spanned {
                    span: 22..26,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 58..100,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 58..77,
                            value: "increment_by_length".to_string(),
                        },
                        t: Spanned {
                            span: 80..100,
                            value: ast::Type::lambda(
                                ast::Type::tuple(vec![
                                    ast::Type::identifier("Int"),
                                    ast::Type::identifier("String"),
                                ]),
                                ast::Type::identifier("Int"),
                            ),
                        },
                    },
                }],
                values: vec![
                    Spanned {
                        span: 113..149,
                        value: Value {
                            name: Spanned {
                                span: 113..132,
                                value: "increment_by_length".to_string(),
                            },
                            expr: Spanned {
                                span: 135..149,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::tuple(vec![
                                        ast::Pattern::int_literal(0),
                                        ast::Pattern::string_literal(r#""""#),
                                    ])],
                                    ast::Expr::int_literal(0),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 162..201,
                        value: Value {
                            name: Spanned {
                                span: 162..181,
                                value: "increment_by_length".to_string(),
                            },
                            expr: Spanned {
                                span: 184..201,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::tuple(vec![
                                        ast::Pattern::identifier("x"),
                                        ast::Pattern::identifier("y"),
                                    ])],
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("x"),
                                        ast::Expr::int_literal(1),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let source = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;
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
                    span: 56..96,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "apply".to_string(),
                        },
                        t: Spanned {
                            span: 64..96,
                            value: ast::Type::lambda(
                                ast::Type::lambda(
                                    ast::Type::identifier("String"),
                                    ast::Type::identifier("Int"),
                                ),
                                ast::Type::lambda(
                                    ast::Type::identifier("String"),
                                    ast::Type::identifier("Int"),
                                ),
                            ),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 109..143,
                    value: Value {
                        name: Spanned {
                            span: 109..114,
                            value: "apply".to_string(),
                        },
                        expr: Spanned {
                            span: 117..143,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::identifier("f")],
                                ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("value")],
                                    ast::Expr::application(
                                        NonEmpty::new("f"),
                                        vec![ast::Expr::identifier("value")],
                                    ),
                                ),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_expr_annotation() {
        let source = r#"
            module Test
            where

            unit_expr = ()"#;
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
                values: vec![Spanned {
                    span: 56..70,
                    value: Value {
                        name: Spanned {
                            span: 56..65,
                            value: "unit_expr".to_string(),
                        },
                        expr: Spanned {
                            span: 68..70,
                            value: ast::Expr::Unit,
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_pattern_annotation() {
        let source = r#"
            module Test
            where

            unit_pattern = |()| -> 0"#;
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
                values: vec![Spanned {
                    span: 56..80,
                    value: Value {
                        name: Spanned {
                            span: 56..68,
                            value: "unit_pattern".to_string(),
                        },
                        expr: Spanned {
                            span: 71..80,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::Unit],
                                ast::Expr::int_literal(0),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_if_then_else() {
        let source = test_source::SIMPLE_IF_THEN_ELSE;
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
                values: vec![
                    Spanned {
                        span: 56..178,
                        value: Value {
                            name: Spanned {
                                span: 56..74,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 77..178,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("num")],
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_positive?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "+",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 191..313,
                        value: Value {
                            name: Spanned {
                                span: 191..209,
                                value: "decrement_negative".to_string(),
                            },
                            expr: Spanned {
                                span: 212..313,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("num")],
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_negative?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "-",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_if_with_no_then_else() {
        let source = r#"
            module Test
            where

            missing_then = |num| ->
              if Number::is_positive?(num) num + 1
              else num
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedThenKeyWord {
            span: 123..126,
            actual: vec![
                Token {
                    kind: TokenKind::LowerIdentifier("num"),
                    span: 123..126,
                },
                Token {
                    kind: TokenKind::Plus,
                    span: 127..128,
                },
                Token {
                    kind: TokenKind::LiteralInt(1),
                    span: 129..130,
                },
                Token {
                    kind: TokenKind::Else,
                    span: 145..149,
                },
                Token {
                    kind: TokenKind::LowerIdentifier("num"),
                    span: 150..153,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 162..162,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_if_then_with_no_else() {
        let source = r#"
            module Test
            where

            missing_then = |num| ->
              if Number::is_positive?(num) then num + 1
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedElseKeyWord {
            span: 144..144,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 144..144,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_if_then_else() {
        let source = test_source::NESTED_IF_THEN_ELSE;
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
                values: vec![Spanned {
                    span: 56..277,
                    value: Value {
                        name: Spanned {
                            span: 56..78,
                            value: "increment_or_decrement".to_string(),
                        },
                        expr: Spanned {
                            span: 81..277,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::identifier("num")],
                                ast::Expr::if_then_else(
                                    ast::Expr::application(
                                        NonEmpty::from((vec!["Number"], "is_positive?")),
                                        vec![ast::Expr::identifier("num")],
                                    ),
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("num"),
                                        ast::Expr::int_literal(1),
                                    ),
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_negative?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "-",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }
}
