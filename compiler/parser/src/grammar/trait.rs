#[allow(clippy::wildcard_imports)]
use super::*;

const TRAIT_RECOVERY_SET: TokenSet = TokenSet::new([TokenKind::TypevarKw, TokenKind::EndKw]);

pub(crate) fn parse_trait(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TraitKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TraitName,
        TRAIT_RECOVERY_SET.plus(TokenKind::WhereKw),
    );
    p.expect_with_recovery(
        TokenKind::WhereKw,
        ParseErrorContext::TraitWhere,
        TRAIT_RECOVERY_SET,
    );

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
        p.maybe_at(TokenKind::EndKw) || p.at_end()
    }
}

enum TraitMemberParseResult {
    TraitMember(CompletedMarker),
    TopLevelKwFound,
    UnknownToken,
}

fn parse_trait_member(p: &mut Parser) -> TraitMemberParseResult {
    if p.at(TokenKind::TypeOfKw) {
        let cm = r#type::parse_type_annotation(p, TRAIT_RECOVERY_SET);
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at(TokenKind::TypevarKw) {
        let cm = parse_trait_type_variable(p);
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at(TokenKind::SelfKw) {
        let cm = parse_trait_self(p);
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at_set(DEFAULT_RECOVERY_SET) {
        TraitMemberParseResult::TopLevelKwFound
    } else {
        TraitMemberParseResult::UnknownToken
    }
}

//
// Type Variables (self and named)
//

fn parse_trait_self(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::SelfKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::TraitSelfConstraintsEquals,
        TRAIT_RECOVERY_SET,
    );

    type_variable::parse_typevar_constraints(p);

    m.complete(p, SyntaxKind::TypeVariable)
}

fn parse_trait_type_variable(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypevarKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        TRAIT_RECOVERY_SET,
    );

    if p.at(TokenKind::Equals) {
        p.bump();

        type_variable::parse_typevar_constraints(p);
    }

    m.complete(p, SyntaxKind::TypeVariable)
}