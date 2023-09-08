#[allow(clippy::wildcard_imports)]
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

        p.expect_with_recovery(TokenKind::DoubleColon, context, recovery_set);
        p.expect_with_recovery(TokenKind::Ident, context, recovery_set);
    }

    return m.complete(p, kind);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(ts![TokenKind::DoubleColon, TokenKind::Colon]) || p.at_end()
    }
}
