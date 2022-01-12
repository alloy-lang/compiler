use core::convert;

use improved_slice_patterns::match_vec;
use itertools::Itertools;

use alloy_lexer::{Token, TokenKind};

use super::{ParseError, ParseResult, Span, Spanned};

pub(crate) fn parse<'a, T, F, C>(
    open_paren_span: Span,
    remainder: &mut (impl Iterator<Item = Token<'a>> + Clone),
    parse_thing: F,
    construct_thing: C,
) -> ParseResult<'a, T>
where
    F: Fn(&Span, Vec<Token<'a>>) -> ParseResult<'a, T>,
    C: FnOnce(Vec<T>) -> T,
{
    let mut pattern_remainder = remainder
        .take_while_ref(|t| !matches!(t.kind, TokenKind::CloseParen))
        .collect::<Vec<_>>();

    let (close_paren_span, remainder) = match_vec!(remainder.collect::<Vec<_>>();
            [
                Token { kind: TokenKind::CloseParen,  span: close_paren_span },
                remainder @ ..
            ] => Ok((close_paren_span, remainder)),

            [remainder @ ..] => {
                let span = open_paren_span.clone();
                let span = span.start..pattern_remainder.iter().next().map(|t| t.span.clone()).unwrap_or(span).end;
                Err(ParseError::ExpectedClosedParen {
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

    // now that we have the open and close parenthesis, we know the total span for the tuple pattern
    let pattern_span = open_paren_span.start..close_paren_span.end;

    let mut pattern_args = Vec::new();
    while !pattern_remainder.is_empty() {
        pattern_remainder = {
            let (pattern_arg, remainder) = parse_thing(&open_paren_span, pattern_remainder)?;
            pattern_args.push(pattern_arg);

            match_vec!(remainder;
                [
                    Token { kind: TokenKind::Comma, span },
                    remainder @ ..
                ] => remainder.collect(),

                [] => Vec::new()
            )
            .map_err(|remaining| ParseError::ExpectedTupleComma {
                span: pattern_span.clone(),
                actual: remaining,
            })?
        };
    }

    Ok((
        Spanned {
            span: pattern_span,
            value: construct_thing(
                pattern_args
                    .into_iter()
                    .map(|t| t.value)
                    .collect::<Vec<_>>(),
            ),
        },
        remainder.collect(),
    ))
}
