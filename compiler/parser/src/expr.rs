use core::convert;

use improved_slice_patterns::match_vec;
use itertools::Itertools;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

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
            Token { kind: TokenKind::Pipe, span: start_pipe_span },
            remainder @ ..
        ] => {
            let mut remainder = remainder.clone();
            let mut pattern_remainder = remainder
                .take_while_ref(|t| !matches!(t.kind, TokenKind::Pipe))
                .collect::<Vec<_>>();

            let (pipe_span, expr, remainder) = match_vec!(remainder.collect::<Vec<_>>();
                [
                    Token { kind: TokenKind::Pipe,  span: end_pipe_span },
                    Token { kind: TokenKind::RightArrow, span: arrow_span },
                    remainder @ ..
                ] => {
                    let pipe_span = start_pipe_span.start..end_pipe_span.end;
                    let (expr, remainder) = parse(&start_pipe_span, remainder)?;
                    Ok((pipe_span, expr, remainder))
                },

                [
                    Token { kind: TokenKind::Pipe,  span: end_pipe_span },
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
                    let span = span.start..pattern_remainder.get(0).map_or(span, |t| t.span.clone()).end;
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
                            Token { kind: TokenKind::Comma, span },
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
        ] => Ok((Spanned {
            span,
            value: ast::Expr::int_literal(i),
        }, Vec::new())),

        [
            Token { kind: TokenKind::LiteralFloat(f), span },
        ] => Ok((Spanned {
            span,
            value: ast::Expr::float_literal(f),
        }, Vec::new())),

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
            Token { kind: TokenKind::OpenParen,           span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, |args| ast::Expr::application(&ast::QualifiedLowerName::from(id), args)),

        [
            Token { kind: TokenKind::LowerPath(id), span },
            Token { kind: TokenKind::OpenParen,           span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, |args| ast::Expr::application(&ast::QualifiedLowerName::from(id), args)),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::identifier(id),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::OpenParen, span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, ast::Expr::tuple),

        [
            Token { kind: TokenKind::If, span: if_span },
            remainder @ ..
        ] => {
            let (if_expr, remainder) = self::parse(&if_span, remainder)?;
            let mut remainder = remainder.into_iter().peekable();

            match remainder.peek().cloned() {
                Some(t) if t.kind == TokenKind::Then => {
                    let (then_expr, remainder) = self::parse(&t.span, remainder.skip(1))?;
                    let mut remainder = remainder.into_iter().peekable();

                    match remainder.peek().cloned() {
                        Some(t) if t.kind == TokenKind::Else => {
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
                Token { kind: TokenKind::Plus, span: op_span },
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
                Token { kind: TokenKind::Minus, span: op_span },
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
