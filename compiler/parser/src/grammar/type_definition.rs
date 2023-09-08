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
    p.expect_with_recovery(TokenKind::Equals, ParseErrorContext::TypeDefEquals, ts![]);

    parse_type_definition_member(p);

    m.complete(p, SyntaxKind::TypeDef)
}

fn parse_type_definition_member(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeDefMemberName,
        ts![],
    );

    loop {
        if should_stop(p) {
            break;
        }

        let m = p.start();
        r#type::parse_type(p, TokenSet::EMPTY, ts![]);
        m.complete(p, SyntaxKind::TypeDefMemberProperty);
    }

    return m.complete(p, SyntaxKind::TypeDefMember);

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TYPEDEF_MEMBER_PROPERTY_FIRSTS) || p.at_end()
    }
}
