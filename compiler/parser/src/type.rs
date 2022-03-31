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
    let (first_type, remainder) = parse_single_type(type_name_span, input)?;

    let mut remainder = remainder.into_iter().peekable();

    let first_span = first_type.span.clone();

    log::debug!("*parse_type* remainder: {:?}", &remainder);

    match remainder.peek() {
        Some(Token { kind, span }) if kind == &TokenKind::RightArrow => {
            let new_span = first_span.start..span.end;

            let (next_type, next_remainder) = parse(&new_span, remainder.skip(1))
                .map_err(|e| match e {
                    ParseError::ExpectedType { span, actual } => {
                        ParseError::ExpectedLambdaReturnType { span, actual }
                    }
                    _ => e,
                })?;

            let span = first_type.span.start..next_type.span.end;
            let value = ast::Type::lambda(first_type.value, next_type.value);
            Ok((Spanned { span, value }, next_remainder))
        }
        Some(Token { kind, span }) if kind == &TokenKind::Pipe => {
            let new_span = first_span.start..span.end;

            let (next_type, next_remainder) = parse(&new_span, remainder.skip(1))?;

            let span = first_type.span.start..next_type.span.end;
            let value = ast::Type::union(vec![first_type.value, next_type.value]);
            Ok((Spanned { span, value }, next_remainder))
        }
        _ => {
            Ok((first_type, remainder.collect()))
        }
    }
}

fn parse_single_type<'a>(
    type_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, ast::Type> {
    let input = input
        .skip_while(|t| matches!(t.kind, TokenKind::Pipe))
        .collect::<Vec<_>>();
    log::debug!("*parse_single_type* input: {:?}", &input);

    match_vec!(input.clone();
        [
            Token { kind: TokenKind::UpperIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned { span, value: ast::Type::identifier(id) }, remainder.collect())),

        // TODO: use-case for this section
        // [
        //     Token { kind: TokenKind::LowerIdentifier(id), span },
        //     remainder @ ..
        // ] => Ok((
        //     Spanned { span, value: ast::Type::variable(id) },
        //     remainder.collect(),
        // )),

        [
            Token { kind: TokenKind::OpenParen, span: open_paren_span },
            remainder @ ..
        ] => parens::parse(open_paren_span, remainder, self::parse_vec, ast::Type::tuple),

        [remainder @ ..,] => Err(ParseError::ExpectedType {
            span: type_span.clone(),
            actual: remainder.collect(),
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF { input, remaining })
    .and_then(convert::identity)
}
