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

    m.complete(p, SyntaxKind::TypeDefinition)
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

        if p.at_top_level_token_or_set(ts![TokenKind::RBracket]) {
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
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(ts![TokenKind::Pipe])
    }

    loop {
        parse_type_definition_member(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(TokenKind::Pipe, ParseErrorContext::TypeDefMemberPipe, ts![]);
    }
}

fn parse_type_definition_member(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_set(ts![TokenKind::EndKw, TokenKind::Pipe])
    }

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
        m.complete(p, SyntaxKind::TypeDefinitionMemberProperty);
    }

    m.complete(p, SyntaxKind::TypeDefinitionMember)
}
