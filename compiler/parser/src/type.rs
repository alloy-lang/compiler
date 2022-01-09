use core::convert;

use improved_slice_patterns::match_vec;
use itertools::Itertools;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::{ParseError, Span, Spanned};

pub fn parse<'a>(
    type_name_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Type>, Vec<Token<'a>>), ParseError<'a>> {
    let (mut type_, remainder) = parse_single_type(type_name_span, input)?;

    let mut remainder = remainder.into_iter().peekable();

    loop {
        log::debug!("*parse_type* remainder: {:?}", &remainder);

        match remainder.peek() {
            Some(Token {
                kind: TokenKind::RightArrow,
                span: arrow_span,
            }) => {
                let new_span = type_.span.start..arrow_span.end;

                let (next_type, next_remainder) = parse_single_type(&new_span, remainder.skip(1))
                    .map_err(|e| match e {
                    ParseError::ExpectedType { span, actual } => {
                        ParseError::ExpectedLambdaReturnType { span, actual }
                    }
                    _ => e,
                })?;

                type_ = Spanned {
                    span: new_span.start..next_type.span.end,
                    value: ast::Type::lambda(type_.value, next_type.value),
                };
                remainder = next_remainder.into_iter().peekable();
            }
            _ => {
                break;
            }
        }
    }

    Ok((type_, remainder.collect()))
}

fn parse_single_type<'a>(
    type_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Type>, Vec<Token<'a>>), ParseError<'a>> {
    let input = input.collect::<Vec<_>>();
    log::debug!("*parse_single_type* input: {:?}", &input);

    match_vec!(input.clone();
        [
            Token { kind: TokenKind::UpperIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned {
            span: span,
            value: ast::Type::Identifier(id.to_string()),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            remainder @ ..
        ] => Ok((
            Spanned { span: span.clone(), value: ast::Type::variable(id) },
            remainder.collect(),
        )),

        [
            Token { kind: TokenKind::OpenParen, span: open_paren_span },
            remainder @ ..
        ] => {
            let mut remainder = remainder.clone();
            let mut type_remainder = remainder
                .take_while_ref(|t| !matches!(t.kind, TokenKind::CloseParen))
                .collect::<Vec<_>>();

            let (close_paren_span, remainder) = match_vec!(remainder.collect::<Vec<_>>();
                [
                    Token { kind: TokenKind::CloseParen,  span: close_paren_span },
                    remainder @ ..
                ] => Ok((close_paren_span, remainder)),

                [remainder @ ..] => {
                    let span = open_paren_span.clone();
                    let span = span.start..type_remainder.iter().next().map(|t| t.span.clone()).unwrap_or(span).end;
                    Err(ParseError::ExpectedClosedParen {
                        span,
                        actual: type_remainder.clone(),
                    })
                }
            )
            .map_err(|remaining| ParseError::ExpectedEOF {
                input: vec![],
                remaining,
            })
            .and_then(convert::identity)?;

            // now that we have the open and close parenthesis, we know the total span for the tuple type
            let type_span = open_paren_span.start..close_paren_span.end;

            let mut type_args = Vec::new();
            while !type_remainder.is_empty() {
                type_remainder = {
                    let (type_arg, remainder) = parse(&open_paren_span, type_remainder.into_iter())?;
                    type_args.push(type_arg);

                    match_vec!(remainder;
                        [
                            Token { kind: TokenKind::Comma, span },
                            remainder @ ..
                        ] => remainder.collect(),

                        [] => Vec::new()
                    )
                    .map_err(|remaining| ParseError::ExpectedTupleComma {
                        span: type_span.clone(),
                        actual: remaining,
                    })?
                };
            }

            Ok((Spanned {
                span: type_span,
                value: ast::Type::tuple(
                    type_args.into_iter().map(|t| t.value).collect::<Vec<_>>(),
                ),
            }, remainder.collect()))
        },

        [remainder @ ..,] => Err(ParseError::ExpectedType {
            span: type_span.clone(),
            actual: remainder.collect(),
        }),
    )
        .map_err(|remaining| ParseError::ExpectedEOF { input, remaining })
        .and_then(convert::identity)
}
