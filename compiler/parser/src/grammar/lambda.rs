#[allow(clippy::wildcard_imports)]
use super::*;

use crate::grammar::argument::parse_argument;
use crate::grammar::expr::parse_expr;

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

        parse_argument(
            p,
            SyntaxKind::LambdaArg,
            ParseErrorContext::LambdaArgExpr,
            ts![],
        );

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
