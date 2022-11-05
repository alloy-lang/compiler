use super::*;

pub(crate) fn parse_trait(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TraitKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TraitName);
    p.expect(TokenKind::WhereKw, ParseErrorContext::TraitWhere);

    loop {
        if should_stop(p) {
            break;
        }

        let member_m = parse_trait_member(p);
        if member_m.is_none() {
            break;
        }
    }

    p.expect(TokenKind::EndKw, ParseErrorContext::TraitEnd);

    return m.complete(p, SyntaxKind::TraitDef);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(TokenSet::new([TokenKind::EndKw])) || p.at_end()
    }
}

fn parse_trait_member(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::TypeOfKw) {
        Some(parse_trait_typeof(p))
    } else {
        None
    }
}

fn parse_trait_typeof(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypeOfKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TypeOfName);
    p.expect(TokenKind::Colon, ParseErrorContext::TypeOfColon);

    parse_type(p);

    m.complete(p, SyntaxKind::TypeOfDef)
}

fn parse_type(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    let _single_type_m = parse_single_type(p);

    if p.at(TokenKind::RightArrow) {
        p.bump();
        parse_type(p);
    }

    m.complete(p, SyntaxKind::Type)
}

fn parse_single_type(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    if p.at(TokenKind::Ident) || p.at(TokenKind::SelfKw) {
        p.bump();
    } else {
        p.error(ParseErrorContext::SingleType);
    }

    m.complete(p, SyntaxKind::SingleType)
}
