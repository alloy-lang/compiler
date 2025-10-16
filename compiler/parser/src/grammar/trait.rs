#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::type_variable::TYPEVAR_CONSTRAINT_FIRSTS;

const TRAIT_RECOVERY_SET: TokenSet = ts![TokenKind::SelfKw, TokenKind::TypevarKw, TokenKind::EndKw];
const TRAIT_MEMBER_FIRSTS: TokenSet = ts![
    TokenKind::TypeOfKw,
    TokenKind::LetKw,
    TokenKind::TypevarKw,
    TokenKind::SelfKw,
];
const TRAIT_TITLE_RECOVERY_SET: TokenSet = TRAIT_RECOVERY_SET.union(ts![TokenKind::WhereKw]);

pub(crate) fn parse_trait(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TRAIT_MEMBER_FIRSTS) && p.at_top_level_token_or_set(ts![TokenKind::EndKw])
    }

    let m = p.start();
    p.bump(TokenKind::TraitKw);

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TraitName,
        TRAIT_TITLE_RECOVERY_SET,
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
            TraitMemberParseResult::TraitMember => {
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

    m.complete(p, SyntaxKind::TraitDef)
}

enum TraitMemberParseResult {
    TraitMember,
    TopLevelKwFound,
    UnknownToken,
}

fn parse_trait_member(p: &mut Parser) -> TraitMemberParseResult {
    if p.at(TokenKind::TypeOfKw) {
        let _cm = type_annotation::parse_type_annotation(
            p,
            r#type::ParseMode::InsideSelfContext,
            TRAIT_RECOVERY_SET,
        );
        TraitMemberParseResult::TraitMember
    } else if p.at(TokenKind::LetKw) {
        let _cm = value::parse_value(p);
        TraitMemberParseResult::TraitMember
    } else if p.at(TokenKind::TypevarKw) {
        let _cm = parse_trait_type_variable(p);
        TraitMemberParseResult::TraitMember
    } else if p.at(TokenKind::SelfKw) {
        let _cm = parse_trait_self(p);
        TraitMemberParseResult::TraitMember
    } else if p.at_set(DEFAULT_RECOVERY_SET) || p.maybe_at(TokenKind::EndKw) {
        TraitMemberParseResult::TopLevelKwFound
    } else {
        TraitMemberParseResult::UnknownToken
    }
}

//
// Type Variables (self and named)
//

fn parse_trait_self(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::SelfKw);

    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::TraitSelfConstraintsEquals,
        TRAIT_RECOVERY_SET.union(TYPEVAR_CONSTRAINT_FIRSTS),
    );

    type_variable::parse_typevar_constraints_with_kind(p);

    m.complete(p, SyntaxKind::SelfTypeVariable)
}

fn parse_trait_type_variable(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::TypevarKw);

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        TRAIT_RECOVERY_SET,
    );

    if p.at(TokenKind::Equals) {
        p.bump(TokenKind::Equals);

        type_variable::parse_typevar_constraints_with_kind(p);
    }
    if p.maybe_at(TokenKind::Ident) {
        p.expect_with_recovery(
            TokenKind::Equals,
            ParseErrorContext::TypeVariableConstraint,
            ts![TokenKind::Ident],
        );

        type_variable::parse_typevar_constraints_with_kind(p);
    }

    m.complete(p, SyntaxKind::NamedTypeVariable)
}
