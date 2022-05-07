use core::convert;

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
        Some(Token { kind, span }) if kind == &T![->] => {
            let new_span = first_span.start..span.end;

            let (next_type, next_remainder) =
                parse(&new_span, remainder.skip(1)).map_err(|e| match e {
                    ParseError::ExpectedType { span, actual } => {
                        ParseError::ExpectedLambdaReturnType { span, actual }
                    }
                    _ => e,
                })?;

            let span = first_type.span.start..next_type.span.end;
            let value = ast::Type::lambda(first_type.value, next_type.value);
            Ok((Spanned { span, value }, next_remainder))
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

        // TODO: use-case for this section
        // [
        //     Token { kind: TokenKind::LowerIdentifier(id), span },
        //     remainder @ ..
        // ] => Ok((
        //     Spanned { span, value: ast::Type::variable(id) },
        //     remainder.collect(),
        // )),

        [
            Token { kind: T!['('], span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, ast::Type::tuple),

        [remainder @ ..,] => Err(ParseError::ExpectedType {
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

    use crate::{parse, Module, ParseError, Spanned, TypeAnnotation};

    #[test]
    fn test_incomplete_function_type_annotation() {
        let source = r#"
            module Test
            where

            incomplete : Int -> 0
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> =
            Err(ParseError::ExpectedLambdaReturnType {
                span: 69..75,
                actual: vec![
                    Token {
                        span: 76..77,
                        kind: TokenKind::LiteralInt(0),
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 86..86,
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
