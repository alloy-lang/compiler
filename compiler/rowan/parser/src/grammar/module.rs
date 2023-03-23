use super::*;

pub(crate) fn parse_module(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::ModuleKw));

    let module_m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::ModuleName,
        TokenSet::new([TokenKind::WhereKw]),
    );
    p.expect(TokenKind::WhereKw, ParseErrorContext::ModuleWhere);

    while !p.at_end() && stmt::stmt(p).is_some() {
        // continue
    }

    module_m.complete(p, SyntaxKind::ModuleDef)
}
