#[allow(clippy::wildcard_imports)]
use super::*;

const BEHAVIOR_RECOVERY_SET: TokenSet = ts![
    TokenKind::TypevarKw,
    TokenKind::EndKw,
];
const BEHAVIOR_TITLE_RECOVERY_SET: TokenSet =
    BEHAVIOR_RECOVERY_SET.union(ts![TokenKind::Ident, TokenKind::ForKw, TokenKind::WhereKw,]);

pub(crate) fn parse_behavior(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::BehaviorKw));

    let m = p.start();
    p.bump();

    r#type::parse_type(
        p,
        ParseErrorContext::BehaviorTraitName,
        r#type::ParseMode::OutsideSelfContext,
        BEHAVIOR_TITLE_RECOVERY_SET,
        ts![],
    );
    p.expect_with_recovery(
        TokenKind::ForKw,
        ParseErrorContext::BehaviorFor,
        BEHAVIOR_TITLE_RECOVERY_SET,
    );
    r#type::parse_type(
        p,
        ParseErrorContext::BehaviorTypeName,
        r#type::ParseMode::OutsideSelfContext,
        BEHAVIOR_TITLE_RECOVERY_SET,
        ts![],
    );
    p.expect_with_recovery(
        TokenKind::WhereKw,
        ParseErrorContext::BehaviorWhere,
        BEHAVIOR_RECOVERY_SET,
    );

    loop {
        if should_stop(p) {
            break;
        }

        let result = parse_trait_member(p);
        match result {
            BehaviorMemberParseResult::BehaviorMember(_) => {
                // empty
            }
            BehaviorMemberParseResult::TopLevelKwFound => {
                break;
            }
            BehaviorMemberParseResult::UnknownToken => {
                p.error_with_recovery(
                    ParseErrorContext::BehaviorMemberFirst,
                    BEHAVIOR_RECOVERY_SET,
                );
            }
        }
    }

    p.expect_only(TokenKind::EndKw, ParseErrorContext::BehaviorEnd);

    return m.complete(p, SyntaxKind::BehaviorDef);

    fn should_stop(p: &mut Parser) -> bool {
        p.maybe_at(TokenKind::EndKw) || p.at_eof()
    }
}

enum BehaviorMemberParseResult {
    BehaviorMember(CompletedMarker),
    TopLevelKwFound,
    UnknownToken,
}

fn parse_trait_member(p: &mut Parser) -> BehaviorMemberParseResult {
    if p.at(TokenKind::TypeOfKw) {
        let cm = type_annotation::parse_type_annotation(
            p,
            r#type::ParseMode::InsideSelfContext,
            BEHAVIOR_RECOVERY_SET,
        );
        BehaviorMemberParseResult::BehaviorMember(cm)
    } else if p.at(TokenKind::LetKw) {
        let cm = variable_def::parse_variable_def(p);
        BehaviorMemberParseResult::BehaviorMember(cm)
    } else if p.at(TokenKind::TypevarKw) {
        let cm = parse_behavior_type_variable(p);
        BehaviorMemberParseResult::BehaviorMember(cm)
    } else if p.at_set(DEFAULT_RECOVERY_SET) {
        BehaviorMemberParseResult::TopLevelKwFound
    } else {
        BehaviorMemberParseResult::UnknownToken
    }
}

fn parse_behavior_type_variable(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypevarKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        BEHAVIOR_RECOVERY_SET,
    );

    if p.at(TokenKind::Equals) {
        p.bump();

        type_variable::parse_typevar_constraints(p);
    }
    if p.maybe_at(TokenKind::Ident) {
        p.expect_with_recovery(
            TokenKind::Equals,
            ParseErrorContext::TypeVariableConstraint,
            ts![TokenKind::Ident],
        );

        type_variable::parse_typevar_constraints_with_kind(p);
    }

    m.complete(p, SyntaxKind::TypeVariable)
}
