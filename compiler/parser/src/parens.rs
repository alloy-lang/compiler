use improved_slice_patterns::match_vec;

use super::{ParseError, ParseResult, Span, Spanned};
use super::{Token, TokenKind};

pub(crate) fn parse_args<'a, T, F>(
    parens_span: &Span,
    remainder: Vec<Token<'a>>,
    parse_thing: F,
) -> Result<Vec<Spanned<T>>, ParseError<'a>>
where
    F: Fn(&Span, Vec<Token<'a>>) -> ParseResult<'a, T>,
{
    let mut parens_args = Vec::new();

    let mut parens_remainder = remainder;
    while !parens_remainder.is_empty() {
        parens_remainder = {
            let (parens_arg, remainder) = parse_thing(&parens_span, parens_remainder)?;
            parens_args.push(parens_arg);

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

    Ok(parens_args)
}
