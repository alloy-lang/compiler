use non_empty_vec::NonEmpty;

use alloy_lexer::{Token, TokenKind};

use crate::NamedType;

use super::r#type;
use super::{ParseError, ParseResult, Span, Spanned};

pub fn parse<'a>(
    type_name_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, NonEmpty<Spanned<NamedType>>> {
    let input = input
        .skip_while(|t| matches!(t.kind, TokenKind::Pipe))
        .peekable();

    let (first_named_type, remainder) = parse_named_type(type_name_span, input)?;

    let mut types = NonEmpty::new(first_named_type);

    let mut remainder = remainder.into_iter().peekable();
    while let Some(Token {
        kind: TokenKind::Pipe,
        span: pipe_span,
    }) = remainder.peek().cloned()
    {
        let (next_named_type, next_remainder) = parse_named_type(&pipe_span, remainder.skip(1))?;
        types.push(next_named_type);
        remainder = next_remainder.into_iter().peekable();
    }

    let (first_type, _) = types.split_first();
    let (last_type, _) = types.split_last();
    let span = first_type.span.start..last_type.span.end;

    let spanned = Spanned { span, value: types };

    Ok((spanned, remainder.collect()))
}

fn parse_named_type<'a>(
    pipe_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, NamedType> {
    let mut input = input.peekable();

    if let Some(Token {
        kind: TokenKind::UpperIdentifier(id),
        span: id_span,
    }) = input.peek().cloned()
    {
        let type_result = r#type::parse(&id_span, input.skip(1));
        let (type_, remainder) = match type_result {
            Ok((type_, remainder)) => (Some(type_), remainder),
            Err(ParseError::ExpectedType { actual, .. }) => (None, actual),
            Err(e) => return Err(e),
        };

        let span = {
            let start = id_span.start;
            let end = type_.clone().map_or_else(|| id_span.end, Spanned::span_end);

            start..end
        };
        let spanned = Spanned {
            span,
            value: NamedType {
                name: Spanned::from_span(id_span, id.into()),
                t: type_,
            },
        };

        Ok((spanned, remainder))
    } else {
        Err(ParseError::ExpectedTypeName {
            span: pipe_span.clone(),
            actual: input.collect::<Vec<_>>(),
        })
    }
}
