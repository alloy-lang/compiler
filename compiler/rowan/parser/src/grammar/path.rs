use super::*;

pub(super) fn parse_path(
    p: &mut Parser,
    context: ParseErrorContext,
    recovery_set: TokenSet,
    kind: SyntaxKind,
) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(TokenKind::DoubleColon, context, TokenSet::EMPTY);
        p.expect_with_recovery(TokenKind::Ident, context, TokenSet::EMPTY);
    }

    return m.complete(p, kind);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::DoubleColon, TokenKind::Colon])) || p.at_end()
    }
}
