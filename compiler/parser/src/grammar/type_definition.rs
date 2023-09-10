#[allow(clippy::wildcard_imports)]
use super::*;

const TYPEDEF_MEMBER_PROPERTY_FIRSTS: TokenSet = ts![TokenKind::LParen, TokenKind::Ident];

pub(crate) fn parse_type_definition(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypedefKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeDefName,
        ts![TokenKind::Equals],
    );

    if p.maybe_at(TokenKind::ClosedAngle) {
        // this is a bit of a hack to allow better error reporting for empty generic constraints
        // 'ClosedAngle' is necessary because otherwise `<>` would be parsed as an OperatorIdentifier
        p.expect_with_recovery(
            TokenKind::Ident,
            ParseErrorContext::TypeDefGenericConstraintName,
            ts![TokenKind::Equals, TokenKind::ClosedAngle],
        );
        p.bump();
    }

    if p.maybe_at(TokenKind::LAngle) {
        parse_type_definition_bounds(p);
    }

    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::TypeDefEquals,
        ts![TokenKind::Ident],
    );

    parse_type_definition_members(p);

    if p.maybe_at(TokenKind::EndKw) {
        p.bump();
    }

    m.complete(p, SyntaxKind::TypeDef)
}

fn parse_type_definition_bounds(p: &mut Parser) {
    assert!(p.at(TokenKind::LAngle));
    p.bump();

    loop {
        let m = p.start();
        p.expect_with_recovery(TokenKind::Ident, ParseErrorContext::BoundedTypeName, ts![]);
        m.complete(p, SyntaxKind::BoundedTypeArg);

        if p.at_set(ts![TokenKind::RAngle]) || p.at_eof() {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::BoundedTypeComma,
            ts![TokenKind::Ident],
        );
    }

    p.expect_with_recovery(
        TokenKind::RAngle,
        ParseErrorContext::BoundedTypeRAngle,
        ts![TokenKind::LBrace],
    );
}

fn parse_type_definition_members(p: &mut Parser) {
    loop {
        parse_type_definition_member(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(TokenKind::Pipe, ParseErrorContext::TypeDefMemberPipe, ts![]);
    }

    return;

    fn should_stop(p: &mut Parser) -> bool {
        !p.maybe_at(TokenKind::Pipe) || p.at_top_level_token() || p.at_eof()
    }
}

fn parse_type_definition_member(p: &mut Parser) -> CompletedMarker {
    if p.maybe_at(TokenKind::Pipe) {
        p.bump();
    }

    let m = p.start();
    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeDefMemberName,
        ts![TokenKind::Pipe],
    );

    loop {
        if should_stop(p) {
            break;
        }

        let m = p.start();
        r#type::parse_type(p, ts![], ts![]);
        m.complete(p, SyntaxKind::TypeDefMemberProperty);
    }

    return m.complete(p, SyntaxKind::TypeDefMember);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TYPEDEF_MEMBER_PROPERTY_FIRSTS) || p.at_top_level_token() || p.at_eof()
    }
}
