use super::*;
use crate::token_set::TokenSet;

const IMPORT_RECOVERY_SET: TokenSet = TokenSet::new([
    // TokenKind::Integer,
    // TokenKind::Fractional,
    // TokenKind::String,
    // TokenKind::Char,
    // TokenKind::Ident,
]);

pub(crate) fn parse_import(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::ImportKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::ImportStatementSegment);

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::DoubleColon,
            ParseErrorContext::ImportStatementSeparator,
            IMPORT_RECOVERY_SET,
        );

        let segment_m = p.start();
        p.expect(TokenKind::Ident, ParseErrorContext::ImportStatementSegment);
        segment_m.complete(p, SyntaxKind::ImportStatementSegment);
    }

    return m.complete(p, SyntaxKind::ImportStatement);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::DoubleColon])) || p.at_end()
    }
}
