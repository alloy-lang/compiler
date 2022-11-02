use super::*;

pub(crate) fn parse_import(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::ImportKw));

    let m = p.start();
    p.bump();

    parse_import_segment(p, ParseErrorContext::ImportStatementFirstSegment);

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::DoubleColon,
            ParseErrorContext::ImportStatementSeparator,
            TokenSet::new([]),
        );

        if p.at(TokenKind::LBrace) {
            parse_import_group(p);

            break;
        }

        parse_import_segment(p, ParseErrorContext::ImportStatementSegment);
    }

    return m.complete(p, SyntaxKind::ImportStatement);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::DoubleColon])) || p.at_end()
    }
}

fn parse_import_group(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LBrace));

    let m = p.start();
    p.bump();

    parse_import_segment(p, ParseErrorContext::ImportStatementSegment);

    loop {
        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ImportStatementGroupSeparator,
            TokenSet::new([TokenKind::Ident]),
        );

        parse_import_segment(p, ParseErrorContext::ImportStatementSegment);
    }

    p.expect_with_recovery(
        TokenKind::RBrace,
        ParseErrorContext::ImportStatementGroupEnd,
        TokenSet::new([]),
    );

    return m.complete(p, SyntaxKind::ImportStatementGroup);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::Comma, TokenKind::Ident])) || p.at_end()
    }
}

fn parse_import_segment(p: &mut Parser, context: ParseErrorContext) -> CompletedMarker {
    let segment_m = p.start();

    p.expect_with_recovery(
        TokenKind::Ident,
        context,
        TokenSet::new([TokenKind::RBrace]),
    );

    segment_m.complete(p, SyntaxKind::ImportStatementSegment)
}
