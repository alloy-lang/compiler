use core::convert;

use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::parens;
use super::{ParseError, ParseResult, Span, Spanned};

fn parse_vec<'a>(type_name_span: &Span, input: Vec<Token<'a>>) -> ParseResult<'a, ast::Type> {
    self::parse(type_name_span, input.into_iter())
}

pub fn parse<'a>(
    type_name_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, ast::Type> {
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
) -> ParseResult<'a, ast::Type> {
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
        ] => parens::parse(open_paren_span, &mut remainder.clone(), self::parse_vec, ast::Type::tuple),

        [remainder @ ..,] => Err(ParseError::ExpectedType {
            span: type_span.clone(),
            actual: remainder.collect(),
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF { input, remaining })
    .and_then(convert::identity)
}
