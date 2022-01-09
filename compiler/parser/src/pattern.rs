use core::convert;

use improved_slice_patterns::match_vec;
use itertools::Itertools;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::{ParseError, Span, Spanned};

pub fn parse<'a>(
    pattern_span: &Span,
    input: Vec<Token<'a>>,
) -> Result<(Spanned<ast::Pattern>, Vec<Token<'a>>), ParseError<'a>> {
    log::debug!("*parse_pattern* input: {:?}", &input);
    // let _doc_comments = extract_doc_comments(input);

    match_vec!(input;
        [
            Token { kind: TokenKind::LiteralInt(i), span },
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::int_literal(i),
        }, Vec::new())),

        [
            Token { kind: TokenKind::LiteralFloat(f), span },
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::float_literal(f),
        }, Vec::new())),

        [
            Token { kind: TokenKind::LiteralInt(i), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::int_literal(i),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LiteralFloat(f), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::float_literal(f),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LiteralString(s), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::string_literal(s),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::LowerIdentifier(id), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Pattern::identifier(id),
        }, remainder.collect())),

        [
            Token { kind: TokenKind::OpenParen, span: open_paren_span },
            remainder @ ..
        ] => parse_tuple_pattern(open_paren_span, &mut remainder.clone()),

        [remainder @ ..] => Err(ParseError::ExpectedPattern {
            span: pattern_span.clone(),
            actual: remainder.collect(),
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF {
        input: vec![],
        remaining,
    })
    .and_then(convert::identity)
}

fn parse_tuple_pattern<'a>(
    open_paren_span: Span,
    remainder: &mut (impl Iterator<Item = Token<'a>> + Clone),
) -> Result<(Spanned<ast::Pattern>, Vec<Token<'a>>), ParseError<'a>> {
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
            let (pattern_arg, remainder) = parse(&open_paren_span, pattern_remainder)?;
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

    Ok((Spanned {
        span: pattern_span,
        value: ast::Pattern::tuple(
            pattern_args.into_iter().map(|t| t.value).collect::<Vec<_>>(),
        ),
    }, remainder.collect()))
}
