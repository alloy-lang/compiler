use core::convert;

use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, T};

use super::parens;
use super::{ParseError, ParseResult, Span, Spanned, TypeVariable};

pub fn parse(input: Vec<Token>) -> Result<(Vec<Spanned<TypeVariable>>, Vec<Token>), ParseError> {
    match input.first() {
        Some(Token {
            kind: T![where],
            span: _,
        }) => inner_parse(input),
        _ => Ok((vec![], input)),
    }
}

fn inner_parse(input: Vec<Token>) -> Result<(Vec<Spanned<TypeVariable>>, Vec<Token>), ParseError> {
    let mut type_variables = vec![];

    let mut remainder: Vec<Token> = input
        .into_iter()
        .skip_while(|t| matches!(t.kind, T![where]))
        .collect();

    let mut cont = !remainder.is_empty();
    while cont {
        log::debug!("*parse_type_variables* remainder: {:?}", &remainder);

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
                remainder @ ..
            ] => {
                cont = false;

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
