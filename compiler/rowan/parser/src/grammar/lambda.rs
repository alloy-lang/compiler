#[allow(clippy::wildcard_imports)]
use super::*;

use crate::grammar::expr::{
    parse_char_literal, parse_expr, parse_fractional_literal, parse_int_literal,
    parse_string_literal, parse_variable_ref,
};

const LAMBDA_ARG_SET: TokenSet = TokenSet::new([
    TokenKind::Integer,
    TokenKind::Fractional,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::Ident,
    TokenKind::LParen,
]);

pub(crate) fn parse_lambda_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));

    let lambda_m = p.start();
    parse_arg_list(p);

    p.expect(
        TokenKind::RightArrow,
        ParseErrorContext::LambdaExprRightArrow,
    );

    let body_m = p.start();
    parse_expr(p, ParseErrorContext::LambdaExprExpr);
    body_m.complete(p, SyntaxKind::LambdaExprBody);

    lambda_m.complete(p, SyntaxKind::LambdaExprDef)
}

fn parse_arg_list(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));
    let m = p.start();
    p.bump();

    loop {
        if should_stop(p) {
            break;
        }

        parse_arg(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::LambdaArgComma,
            LAMBDA_ARG_SET,
        );
    }

    p.expect_with_recovery(
        TokenKind::Pipe,
        ParseErrorContext::LambdaArgPipe,
        ts![TokenKind::RightArrow],
    );

    return m.complete(p, SyntaxKind::LambdaArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(ts![TokenKind::Pipe, TokenKind::RightArrow]) || p.at_end()
    }
}

fn parse_arg(p: &mut Parser) -> CompletedMarker {
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
    } else {
        p.error(ParseErrorContext::LambdaArgExpr);
    }

    m.complete(p, SyntaxKind::LambdaArg)
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

        parse_arg(p);
        arg_len += 1;

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ParenExprComma,
            LAMBDA_ARG_SET,
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
        p.at_set(ts![TokenKind::RParen]) || p.at_end()
    }
}
