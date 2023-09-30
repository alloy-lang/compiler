#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::type_variable::TYPEVAR_CONSTRAINT_FIRSTS;

const TRAIT_RECOVERY_SET: TokenSet = ts![TokenKind::SelfKw, TokenKind::TypevarKw, TokenKind::EndKw];
const TRAIT_TITLE_RECOVERY_SET: TokenSet = TRAIT_RECOVERY_SET.union(ts![TokenKind::WhereKw]);

pub(crate) fn parse_trait(p: &mut Parser) -> CompletedMarker {
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
        p.maybe_at(TokenKind::EndKw) || p.at_eof()
    }
}

enum TraitMemberParseResult {
    TraitMember(CompletedMarker),
    TopLevelKwFound,
    UnknownToken,
}

fn parse_trait_member(p: &mut Parser) -> TraitMemberParseResult {
    if p.at(TokenKind::TypeOfKw) {
        let cm = type_annotation::parse_type_annotation(
            p,
            r#type::ParseMode::InsideSelfContext,
            TRAIT_RECOVERY_SET,
        );
        TraitMemberParseResult::TraitMember(cm)
    } else if p.at(TokenKind::LetKw) {
        let cm = value::parse_value(p);
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
