#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::expr::EXPR_FIRSTS;

pub(crate) fn parse_variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    ident::parse_ident_or_op(
        p,
        ParseErrorContext::VariableDefIdent,
        ts![TokenKind::Equals],
    );
    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::VariableDefEquals,
        EXPR_FIRSTS,
    );

    expr::parse_expr(p, ParseErrorContext::VariableDefExpr);

    m.complete(p, SyntaxKind::VariableDef)
}
