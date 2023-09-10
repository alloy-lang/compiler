#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::expr::{
    parse_char_literal, parse_fractional_literal, parse_int_literal, parse_string_literal,
};

pub(crate) const ARGUMENT_RECOVERY_SET: TokenSet = TokenSet::new([
    TokenKind::Integer,
    TokenKind::Fractional,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::Ident,
    TokenKind::LParen,
    TokenKind::NilIdentifier,
]);

pub(crate) fn parse_argument(
    p: &mut Parser,
    kind: SyntaxKind,
    context: ParseErrorContext,
    recovery_set: TokenSet,
) -> CompletedMarker {
    let m = p.start();

    if p.at(TokenKind::Integer) {
        parse_int_literal(p);
    } else if p.at(TokenKind::Fractional) {
        parse_fractional_literal(p);
    } else if p.at(TokenKind::String) {
        parse_string_literal(p);
    } else if p.at(TokenKind::Char) {
        parse_char_literal(p);
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p);
    } else if p.at(TokenKind::LParen) {
        parse_tuple_arg(p);
    } else if p.at(TokenKind::NilIdentifier) {
        p.bump();
    } else {
        p.error_with_recovery(context, recovery_set);
    }

    m.complete(p, kind)
}

fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    path::parse_path(
        p,
        ParseErrorContext::VariableRef,
        TokenSet::EMPTY,
        SyntaxKind::VariableRef,
    )
}

fn parse_tuple_arg(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let paren_m = p.start();
    p.bump();

    let mut arg_len = 0;
    loop {
        if should_stop(p) {
            break;
        }

        parse_argument(
            p,
            SyntaxKind::TupleExprArg,
            ParseErrorContext::ParenExprExpr,
            ts![TokenKind::RightArrow],
        );
        arg_len += 1;

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ParenExprComma,
            ARGUMENT_RECOVERY_SET,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::ParenExprRightParen);

    let kind = match arg_len {
        0 => SyntaxKind::UnitExpr,
        1 => SyntaxKind::ParenExpr,
        _ => SyntaxKind::TupleExpr,
    };
    return paren_m.complete(p, kind);

    fn should_stop(p: &mut Parser) -> bool {
        p.maybe_at(TokenKind::RParen) || p.at_eof()
    }
}
