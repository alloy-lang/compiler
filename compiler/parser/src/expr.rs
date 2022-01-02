use improved_slice_patterns::match_vec;
use itertools::Itertools;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::pattern;
use super::{ParseError, Span, Spanned};

pub fn parse<'a>(
    expr_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Expr>, Vec<Token<'a>>), ParseError<'a>> {
    let input = input.collect::<Vec<_>>();
    log::debug!("*parse_expr* input: {:?}", &input);

    // let _doc_comments = extract_doc_comments(stream);

    match_vec!(input.clone();
        [
            Token { kind: TokenKind::Pipe, span: pipe_span },
            remainder @ ..
        ] => {
            let mut remainder = remainder.clone();
            let mut pattern_remainder = remainder
                .take_while_ref(|t| !matches!(t.kind, TokenKind::Pipe))
                .collect::<Vec<_>>();

            let (expr, remainder) = match_vec!(remainder.collect::<Vec<_>>();
                [
                    Token { kind: TokenKind::Pipe,  span: end_pipe_span },
                    Token { kind: TokenKind::Arrow, span: arrow_span },
                    remainder @ ..
                ] => parse(&pipe_span, remainder),

                [remainder @ ..,] => Err(ParseError::ExpectedPipe {
                    span: expr_span.clone(),
                    actual: remainder.collect(),
                })
            )
            .map_err(|remaining| ParseError::ExpectedEOF {
                input: vec![],
                remaining,
            })
            .and_then(|s| s)?;

            let mut args = Vec::new();
            while !pattern_remainder.is_empty() {
                pattern_remainder = {
                    let (pattern, remainder) = pattern::parse(&pipe_span, pattern_remainder.clone())?;
                    args.push(pattern);

                    remainder
                };
            }

            Ok((Spanned {
                span: pipe_span.start..expr.span.end,
                value: ast::Expr::lambda(
                    args.into_iter().map(|t| t.value).collect::<Vec<_>>(),
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
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::identifier(id),
        }, remainder.collect())),

        [remainder @ ..,] => Err(ParseError::ExpectedExpr {
            span: expr_span.clone(),
            actual: remainder.collect(),
        }),
    )
        .map_err(|remaining: Vec<Token<'a>>| ParseError::ExpectedEOF {
            input,
            remaining,
        })
        .and_then(|s| s)
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

            [remainder @ ..] => Ok(
                (expr1.clone(), remainder.collect())
            ),
        )
            .and_then(|s| s)
            .or_else(|remainder| Ok((expr1, remainder)))
        )
}
