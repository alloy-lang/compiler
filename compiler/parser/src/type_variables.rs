use core::convert;

use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, T};

use super::parens;
use super::{ParseError, ParseResult, Span, Spanned, TypeVariable};

pub fn parse<'a>(
    starting_type_span: &Span,
    input: Vec<Token<'a>>,
) -> Result<(Vec<Spanned<TypeVariable>>, Vec<Token<'a>>), ParseError<'a>> {
    match input.first() {
        Some(Token { kind: T![where], span: _ }) => inner_parse(starting_type_span, input),
        _ => Ok((vec![], input)),
    }
}

fn inner_parse<'a>(
    starting_type_span: &Span,
    input: Vec<Token<'a>>,
) -> Result<(Vec<Spanned<TypeVariable>>, Vec<Token<'a>>), ParseError<'a>> {
    let mut type_variables = vec![];

    let mut furthest_character_position = starting_type_span.end;

    let mut remainder = &input[1..];

    let mut cont = !remainder.is_empty();
    while cont {
        dbg!("*parse_type_variables* remainder: {:?}", &remainder);

        remainder = match_vec!(remainder;
            [
                Token { kind: T![typevar],                    span: typevar_token_span },
                Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                remainder @ ..
            ] => {
                type_variables.push(Spanned {
                    span: id_span,
                    value: TypeVariable::new_free(id),
                });

                Ok(remainder.collect())
            },

            [
                Token { kind: T![end],  span: end_marker_span },
                remainder @ ..
            ] => {
                cont = false;
                furthest_character_position = end_marker_span.end;

                Ok(remainder.collect())
            },
        )
            .map_err(|remaining| ParseError::ExpectedEOF {
                input: vec![],
                remaining,
            })
            .and_then(convert::identity)?;

        cont = cont && !remainder.is_empty();
    }

    Ok((type_variables, remainder))
}