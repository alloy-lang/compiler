use alloy_lexer::{Token, TokenKind};
use itertools::Itertools;

use super::{ParseError, ParseResult, Span, Spanned};

pub(crate) fn parse<'a, T, F, C>(
    open_paren_span: Span,
    remainder: impl Iterator<Item = Token<'a>> + Clone,
    parse_thing: F,
    construct_thing: C,
) -> ParseResult<'a, T>
where
    F: Fn(&Span, Vec<Token<'a>>) -> ParseResult<'a, T>,
    C: FnOnce(Vec<T>) -> T,
{
    let mut parens_remainder = remainder.collect_vec().into_iter().peekable();
    let mut parens_span = open_paren_span;
    let mut parens_args = Vec::new();
    loop {
        match parens_remainder.peek().map(|t| t.kind.clone()) {
            None | Some(TokenKind::CloseParen | TokenKind::EOF) => break,
            _ => {}
        };

        parens_remainder = {
            let (spanned, remainder) = parse_thing(&parens_span, parens_remainder.collect())?;
            let Spanned {
                value: inner_value,
                span: inner_span,
            } = spanned;

            parens_span = parens_span.start..inner_span.end;
            parens_args.push(inner_value);

            let mut remainder = remainder.into_iter().peekable();

            match remainder.peek().cloned() {
                Some(t) if t.kind == TokenKind::Comma => {
                    remainder.next();
                    remainder
                }
                Some(t) if t.kind == TokenKind::CloseParen || t.kind == TokenKind::EOF => remainder,
                _ => {
                    return Err(ParseError::ExpectedTupleComma {
                        span: parens_span.clone(),
                        actual: remainder.collect(),
                    })
                }
            }
        };
    }

    let (close_paren_span, remainder) = match parens_remainder.peek().cloned() {
        Some(t) if t.kind == TokenKind::CloseParen => {
            Ok((t.span, parens_remainder.skip(1).collect_vec()))
        }
        _ => {
            let span = parens_span.clone();
            let span = span.start..parens_remainder.peek().map_or(span, |t| t.span.clone()).end;
            Err(ParseError::ExpectedClosedParen {
                span,
                actual: parens_remainder.collect(),
            })
        }
    }?;

    Ok((
        Spanned {
            span: parens_span.start..close_paren_span.end,
            value: construct_thing(parens_args),
        },
        remainder,
    ))
}