#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(crate) enum ParseMode {
    InsideKindContext,
    OutsideKindContext,
}

pub(crate) const TYPEVAR_CONSTRAINT_FIRSTS: TokenSet =
    ts![TokenKind::Hash, TokenKind::Ident, TokenKind::Plus];

pub(crate) fn parse_typevar_constraints(p: &mut Parser) {
    inner_parse_typevar_constraints(p, ParseMode::OutsideKindContext);
}

pub(crate) fn parse_typevar_constraints_with_kind(p: &mut Parser) {
    inner_parse_typevar_constraints(p, ParseMode::InsideKindContext);
}

fn inner_parse_typevar_constraints(p: &mut Parser, mode: ParseMode) {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(TYPEVAR_CONSTRAINT_FIRSTS)
    }

    loop {
        parse_typevar_constraint(p, mode);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Plus,
            ParseErrorContext::TypeVariableConstraintPlus,
            TYPEVAR_CONSTRAINT_FIRSTS,
        );
    }
}

fn parse_typevar_constraint(p: &mut Parser, mode: ParseMode) {
    if mode == ParseMode::InsideKindContext && p.at(TokenKind::Hash) {
        parse_typevar_constraint_kind_marker(p);
    } else if p.at(TokenKind::Ident) {
        parse_typevar_constraint_trait_marker(p);
    } else {
        p.error_with_recovery(
            ParseErrorContext::TypeVariableConstraint,
            ts![TokenKind::TypevarKw, TokenKind::EndKw],
        );
    }
}

const TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY: TokenSet = TYPEVAR_CONSTRAINT_FIRSTS
    .plus(TokenKind::LBracket)
    .plus(TokenKind::NilIdentifier)
    .plus(TokenKind::RBracket);
const TYPEVAR_CONSTRAINT_KIND_MARKER_CONTINUE: TokenSet =
    ts![TokenKind::Comma, TokenKind::NilIdentifier];

fn parse_typevar_constraint_kind_marker(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(TYPEVAR_CONSTRAINT_KIND_MARKER_CONTINUE)
    }

    let m = p.start();
    p.bump(TokenKind::Hash);

    p.expect_with_recovery(
        TokenKind::TypeKw,
        ParseErrorContext::TypeVariableKindConstraintTypeKw,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.minus(TokenKind::Ident),
    );

    p.expect_with_recovery(
        TokenKind::LBracket,
        ParseErrorContext::TypeVariableKindConstraintLBracket,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
    );
    p.expect_with_recovery(
        TokenKind::NilIdentifier,
        ParseErrorContext::TypeVariableKindConstraintUnderscore,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.plus(TokenKind::TypevarKw),
    );

    if p.at_set(TYPEVAR_CONSTRAINT_KIND_MARKER_CONTINUE) {
        loop {
            if should_stop(p) {
                break;
            }

            p.expect_with_recovery(
                TokenKind::Comma,
                ParseErrorContext::TypeVariableKindConstraintUnderscoreComma,
                TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
            );

            if should_stop(p) {
                break;
            }

            p.expect_with_recovery(
                TokenKind::NilIdentifier,
                ParseErrorContext::TypeVariableKindConstraintUnderscore,
                TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
            );
        }
    }

    p.expect_with_recovery(
        TokenKind::RBracket,
        ParseErrorContext::TypeVariableKindConstraintRBracket,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.plus(TokenKind::TypevarKw),
    );

    m.complete(p, SyntaxKind::TypeVariableKindConstraint)
}

fn parse_typevar_constraint_trait_marker(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    path::parse_path(
        p,
        ParseErrorContext::TypeVariableTraitConstraint,
        ts![TokenKind::TypevarKw],
        SyntaxKind::TypeVariableTraitConstraint,
    )
}
