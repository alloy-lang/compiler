#[allow(clippy::wildcard_imports)]
use super::*;

const BEHAVIOR_RECOVERY_SET: TokenSet = ts![
    TokenKind::EndKw,
];
const BEHAVIOR_TITLE_RECOVERY_SET: TokenSet =
    BEHAVIOR_RECOVERY_SET.union(ts![TokenKind::Ident, TokenKind::ForKw, TokenKind::WhereKw,]);

pub(crate) fn parse_behavior(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::BehaviorKw));

    let m = p.start();
    p.bump();

    path::parse_path(
        p,
        ParseErrorContext::BehaviorTraitName,
        BEHAVIOR_TITLE_RECOVERY_SET,
        SyntaxKind::TypeIdentifier,
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

    p.expect_only(TokenKind::EndKw, ParseErrorContext::BehaviorEnd);

    return m.complete(p, SyntaxKind::BehaviorDef);

    fn should_stop(p: &mut Parser) -> bool {
        p.maybe_at(TokenKind::EndKw) || p.at_eof()
    }
}
