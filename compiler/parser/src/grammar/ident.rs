#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn parse_ident_or_op(
    p: &mut Parser,
    context: ParseErrorContext,
    recovery_set: TokenSet,
) {
    if p.at(TokenKind::OpIdent) {
        p.expect_with_recovery(TokenKind::OpIdent, context, recovery_set);
    } else {
        p.expect_with_recovery(TokenKind::Ident, context, recovery_set);
    }
}
