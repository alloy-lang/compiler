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
        TokenSet::new([TokenKind::RightArrow]),
    );

    return m.complete(p, SyntaxKind::LambdaArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(TokenSet::new([TokenKind::Pipe, TokenKind::RightArrow])) || p.at_end()
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
    } else {
        p.error(ParseErrorContext::LambdaArgExpr);
    }

    m.complete(p, SyntaxKind::LambdaArg)
}
