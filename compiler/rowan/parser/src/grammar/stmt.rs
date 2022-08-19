use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(variable_def(p))
    } else {
        expr::parse_expr(p, ParseErrorContext::TopLevelExpr)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::VariableDefIdent);
    p.expect(TokenKind::Equals, ParseErrorContext::VariableDefEquals);

    expr::parse_expr(p, ParseErrorContext::VariableDefExpr);

    m.complete(p, SyntaxKind::VariableDef)
}
