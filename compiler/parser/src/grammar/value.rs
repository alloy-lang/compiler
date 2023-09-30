#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::expr::EXPR_FIRSTS;

pub(crate) fn parse_value(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::LetKw);

    ident::parse_ident_or_op(p, ParseErrorContext::ValueDefIdent, ts![TokenKind::Equals]);
    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::ValueDefEquals,
        EXPR_FIRSTS,
    );

    expr::parse_expr(p, ParseErrorContext::ValueDefExpr);

    m.complete(p, SyntaxKind::ValueDef)
}
