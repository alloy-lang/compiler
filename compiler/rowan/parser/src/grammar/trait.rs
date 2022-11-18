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
        Some(parse_trait_type_annotation(p))
    } else {
        None
    }
}

fn parse_trait_type_annotation(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypeOfKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TypeOfName);
    p.expect(TokenKind::Colon, ParseErrorContext::TypeOfColon);

    parse_type(p);

    if p.at(TokenKind::WhereKw) {
        parse_trait_type_annotation_typevars(p);
    }

    m.complete(p, SyntaxKind::TypeAnnotation)
}

fn parse_trait_type_annotation_typevars(p: &mut Parser) {
    assert!(p.at(TokenKind::WhereKw));
    p.bump();

    loop {
        if !p.at_set(TokenSet::new([TokenKind::TypevarKw])) {
            break;
        }

        parse_trait_type_annotation_typevar(p);
    }
}

fn parse_trait_type_annotation_typevar(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypevarKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TypeVariableName);

    m.complete(p, SyntaxKind::TypeVariable)
}

fn parse_type(p: &mut Parser) -> Option<CompletedMarker> {
    let single_type_m = parse_single_type(p)?;

    if p.at(TokenKind::RightArrow) {
        let m = single_type_m.precede(p);

        p.bump();
        parse_type(p);

        return Some(m.complete(p, SyntaxKind::LambdaType));
    }

    Some(single_type_m)
}

fn parse_single_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump();

        return Some(m.complete(p, SyntaxKind::TypeIdentifier));
    } else if p.at(TokenKind::SelfKw) {
        let m = p.start();
        p.bump();

        return Some(m.complete(p, SyntaxKind::SelfType));
        // } else if p.at(TokenKind::LParen) {
        //     p.bump();
        //     parse_type(p);
        //     p.expect(TokenKind::RParen, ParseErrorContext::TypeRightParen);
    } else {
        p.error(ParseErrorContext::SingleType);
    }

    None
}
