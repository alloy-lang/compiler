use core::convert;

use improved_slice_patterns::match_vec;

use alloy_lexer::{Token, TokenKind, T};

use super::{ParseError, Spanned, TypeConstraint, TypeVariable};

pub fn parse(input: Vec<Token>) -> Result<(Vec<Spanned<TypeVariable>>, Vec<Token>), ParseError> {
    match input.first() {
        Some(Token {
            kind: T![where], ..
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
                Token { kind: T![=],                          span: eq_span },
                remainder @ ..
            ] => {
                let (type_constraints, furthest_character_position, remainder) = parse_type_constraints(remainder.collect(), eq_span.end)?;

                type_variables.push(Spanned {
                    span: typevar_token_span.start..furthest_character_position,
                    value: TypeVariable {
                        id: Spanned {
                            value: id.to_string(),
                            span: id_span,
                        },
                        constraints: type_constraints,
                    },
                });

                Ok(remainder)
            },

            [
                Token { kind: T![typevar],                    span: typevar_token_span },
                Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                remainder @ ..
            ] => {
                type_variables.push(Spanned {
                    span: typevar_token_span.start..id_span.end,
                    value: TypeVariable::new_free(id, id_span),
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

fn parse_type_constraints(
    remainder: Vec<Token>,
    furthest_character_position: usize,
) -> Result<(Vec<Spanned<TypeConstraint>>, usize, Vec<Token>), ParseError> {
    let mut furthest_character_position = furthest_character_position;
    let mut remainder = remainder;
    let mut type_constraints = Vec::new();

    let mut cont = true;
    while cont {
        remainder = match_vec!(remainder.clone();
            [
                Token { kind: TokenKind::KindMarker(args), span: kind_marker_span },
                remainder @ ..
            ] => {
                furthest_character_position = kind_marker_span.end;
                type_constraints.push(Spanned {
                    span: kind_marker_span,
                    value: TypeConstraint::new_kind(args),
                });

                remainder.collect()
            },

            [
                Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                remainder @ ..
            ] => {
                furthest_character_position = id_span.end;
                type_constraints.push(Spanned {
                    span: id_span,
                    value: TypeConstraint::Trait(id.to_string()),
                });

                remainder.collect()
            },

            [
                Token { kind: T![+], span: plus_span },
                remainder @ ..
            ] => {
                furthest_character_position = plus_span.end;
                remainder.collect()
            },

            [
                remainder @ ..
            ] => {
                cont = false;
                remainder.collect()
            }
        )
        .map_err(|_remaining| todo!("ParseError: did not match anything...wat? 1234"))?;
        // .map_err(|remaining| ParseError::ExpectedLambdaArgsComma {
        //     span: eq_span.clone(),
        //     actual: remaining,
        // })?;
    }

    Ok((type_constraints, furthest_character_position, remainder))
}
