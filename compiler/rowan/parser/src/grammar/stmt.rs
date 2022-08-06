use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(variable_def(p))
    } else {
        expr::parse_expr(p)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Equals);

    expr::parse_expr(p);

    m.complete(p, SyntaxKind::VariableDef)
}
