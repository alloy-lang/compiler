use super::*;

pub(crate) fn parse_trait(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TraitKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TraitName);
    p.expect(TokenKind::WhereKw, ParseErrorContext::TraitWhere);

    p.expect(TokenKind::EndKw, ParseErrorContext::TraitEnd);

    return m.complete(p, SyntaxKind::TraitDef);
}
