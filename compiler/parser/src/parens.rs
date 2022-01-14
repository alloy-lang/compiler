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
    let mut parens_remainder = remainder
        .take_while_ref(|t| !matches!(t.kind, TokenKind::CloseParen))
        .collect::<Vec<_>>();

    let (close_paren_span, remainder) = match_vec!(remainder.collect::<Vec<_>>();
            [
                Token { kind: TokenKind::CloseParen,  span: close_paren_span },
                remainder @ ..
            ] => Ok((close_paren_span, remainder)),

            [remainder @ ..] => {
                let span = open_paren_span.clone();
                let span = span.start..parens_remainder.iter().next().map(|t| t.span.clone()).unwrap_or(span).end;
                Err(ParseError::ExpectedClosedParen {
                    span,
                    actual: parens_remainder.clone(),
                })
            }
        )
        .map_err(|remaining| ParseError::ExpectedEOF {
            input: vec![],
            remaining,
        })
        .and_then(convert::identity)?;

    // now that we have the open and close parenthesis, we know the total span for the tuple parens
    let parens_span = open_paren_span.start..close_paren_span.end;

    let mut parens_args = Vec::new();
    while !parens_remainder.is_empty() {
        parens_remainder = {
            let (parens_arg, remainder) = parse_thing(&open_paren_span, parens_remainder)?;
            parens_args.push(parens_arg.value);

            match_vec!(remainder;
                [
                    Token { kind: TokenKind::Comma, span },
                    remainder @ ..
                ] => remainder.collect(),

                [] => Vec::new()
            )
            .map_err(|remaining| ParseError::ExpectedTupleComma {
                span: parens_span.clone(),
                actual: remaining,
            })?
        };
    }

    Ok((
        Spanned {
            span: parens_span,
            value: construct_thing(parens_args),
        },
        remainder.collect(),
    ))
}
