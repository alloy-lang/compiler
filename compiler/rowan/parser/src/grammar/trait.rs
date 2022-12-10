use crate::parser::DEFAULT_RECOVERY_SET;
use super::*;

const TRAIT_RECOVERY_SET: TokenSet =
    TokenSet::new([TokenKind::TypevarKw, TokenKind::TypeOfKw, TokenKind::EndKw]);

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

        let result = parse_trait_member(p);
        match result {
            TraitMemberParseResult::TraitMember(_) => {
                // empty
            }
            TraitMemberParseResult::TopLevelKwFound => {
                break;
            }
            TraitMemberParseResult::UnknownToken => {
                p.error_with_recovery(ParseErrorContext::TraitMemberFirst, TRAIT_RECOVERY_SET);
            }
        }
    }

    p.expect_only(TokenKind::EndKw, ParseErrorContext::TraitEnd);

    return m.complete(p, SyntaxKind::TraitDef);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(TokenSet::new([TokenKind::EndKw])) || p.at_end()
    }
}

enum TraitMemberParseResult {
    TraitMember(CompletedMarker),
    TopLevelKwFound,
    UnknownToken,
}

fn parse_trait_member(p: &mut Parser) -> TraitMemberParseResult {
    if p.at(TokenKind::TypeOfKw) {
        let cm = parse_trait_type_annotation(p);
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at(TokenKind::TypevarKw) {
        let cm = parse_trait_typevar(p);
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at_set(DEFAULT_RECOVERY_SET) {
        TraitMemberParseResult::TopLevelKwFound
    } else {
        TraitMemberParseResult::UnknownToken
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

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        TRAIT_RECOVERY_SET,
    );

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
    } else if p.at(TokenKind::LParen) {
        return Some(parse_parenthesized_type(p));
    } else {
        p.error(ParseErrorContext::SingleType);
    }

    None
}

fn parse_parenthesized_type(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::UnitType);
    }
    if p.at_set(TRAIT_RECOVERY_SET) {
        p.error_with_recovery(ParseErrorContext::UnitTypeRightParen, TRAIT_RECOVERY_SET);
        return m.complete(p, SyntaxKind::UnitType);
    }

    parse_type(p);

    let mut comma_count = 0;
    if p.at(TokenKind::Comma) {
        loop {
            if should_stop(p) {
                break;
            }

            comma_count += 1;

            if p.at(TokenKind::Comma) {
                p.bump();
            }

            parse_type(p);
        }
    }

    p.expect_with_recovery(TokenKind::RParen, ParseErrorContext::UnitTypeRightParen, TRAIT_RECOVERY_SET);

    return if comma_count == 0 {
        m.complete(p, SyntaxKind::ParenthesizedType)
    } else {
        m.complete(p, SyntaxKind::TupleType)
    };

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::Comma])) || p.at_end()
    }
}

fn parse_trait_typevar(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypevarKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        TRAIT_RECOVERY_SET,
    );

    m.complete(p, SyntaxKind::TypeVariable)
}
