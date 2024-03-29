#[allow(clippy::wildcard_imports)]
use super::*;

const IMPORT_RECOVERY_SET: TokenSet = ts![TokenKind::Ident, TokenKind::LBrace];

pub(crate) fn parse_import(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(IMPORT_RECOVERY_SET.plus(TokenKind::DoubleColon))
    }

    let m = p.start();
    p.bump(TokenKind::ImportKw);

    parse_first_import_segment(p, ParseErrorContext::ImportDefFirstSegment);

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::DoubleColon,
            ParseErrorContext::ImportDefSeparator,
            IMPORT_RECOVERY_SET,
        );

        if p.at(TokenKind::LBrace) {
            parse_import_group(p);
        } else {
            parse_import_segment(p, ParseErrorContext::ImportDefSegment);
        }
    }

    m.complete(p, SyntaxKind::ImportDef)
}

fn parse_import_group(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(ts![TokenKind::Comma, TokenKind::Ident])
    }

    let m = p.start();
    p.bump(TokenKind::LBrace);

    parse_import_segment(p, ParseErrorContext::ImportDefSegment);

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ImportDefGroupSeparator,
            ts![TokenKind::Ident],
        );

        parse_import_segment(p, ParseErrorContext::ImportDefSegment);
    }

    p.expect_with_recovery(
        TokenKind::RBrace,
        ParseErrorContext::ImportDefGroupEnd,
        ts![],
    );

    m.complete(p, SyntaxKind::ImportDefGroup)
}

const IMPORT_SEGMENT_RECOVERY_SET: TokenSet = ts![TokenKind::RBrace];

fn parse_first_import_segment(p: &mut Parser, context: ParseErrorContext) -> CompletedMarker {
    let segment_m = p.start();

    p.expect_with_recovery(TokenKind::Ident, context, IMPORT_SEGMENT_RECOVERY_SET);

    segment_m.complete(p, SyntaxKind::ImportDefSegment)
}

fn parse_import_segment(p: &mut Parser, context: ParseErrorContext) -> CompletedMarker {
    let segment_m = p.start();

    if p.at(TokenKind::OpIdent) {
        p.expect_with_recovery(TokenKind::OpIdent, context, IMPORT_SEGMENT_RECOVERY_SET);
    } else {
        p.expect_with_recovery(TokenKind::Ident, context, IMPORT_SEGMENT_RECOVERY_SET);
    }

    segment_m.complete(p, SyntaxKind::ImportDefSegment)
}
