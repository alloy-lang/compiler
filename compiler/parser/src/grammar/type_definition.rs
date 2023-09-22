#[allow(clippy::wildcard_imports)]
use super::*;

pub(crate) fn parse_type_definition(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::TypedefKw);

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeDefName,
        ts![TokenKind::Equals],
    );

    if p.maybe_at(TokenKind::LBracket) {
        parse_type_definition_bounds(p);
    }

    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::TypeDefEquals,
        ts![TokenKind::Ident],
    );

    parse_type_definition_members(p);

    if p.maybe_at(TokenKind::EndKw) {
        p.bump(TokenKind::EndKw);
    }

    m.complete(p, SyntaxKind::TypeDef)
}

fn parse_type_definition_bounds(p: &mut Parser) {
    p.bump(TokenKind::LBracket);

    loop {
        let m = p.start();
        p.expect_with_recovery(
            TokenKind::Ident,
            ParseErrorContext::BoundedTypeArgName,
            ts![TokenKind::RBracket, TokenKind::Comma],
        );
        m.complete(p, SyntaxKind::BoundedTypeArg);

        if p.maybe_at(TokenKind::RBracket) || p.at_eof() {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::BoundedTypeComma,
            ts![TokenKind::Ident],
        );
    }

    p.expect_with_recovery(
        TokenKind::RBracket,
        ParseErrorContext::BoundedTypeRBracket,
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
        p.bump(TokenKind::Pipe);
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
        r#type::parse_type(
            p,
            ParseErrorContext::TypeDefMemberPropertyType,
            r#type::ParseMode::OutsideSelfContext,
            r#type::SINGLE_TYPE_RECOVERY_SET,
            ts![],
        );
        m.complete(p, SyntaxKind::TypeDefMemberProperty);
    }

    return m.complete(p, SyntaxKind::TypeDefMember);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(ts![TokenKind::EndKw, TokenKind::Pipe]) || p.at_top_level_token() || p.at_eof()
    }
}
