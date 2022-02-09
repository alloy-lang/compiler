use core::convert;

use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::parens;
use super::{ParseError, ParseResult, Span, Spanned};

pub fn parse<'a>(pattern_span: &Span, input: Vec<Token<'a>>) -> ParseResult<'a, ast::Pattern> {
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
        ] => parens::parse(open_paren_span, remainder, self::parse, ast::Pattern::tuple),

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
