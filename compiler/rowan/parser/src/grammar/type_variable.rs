use super::*;

const TYPEVAR_CONSTRAINT_FIRSTS: TokenSet =
    TokenSet::new([TokenKind::Hash, TokenKind::Ident, TokenKind::Plus]);

pub(crate) fn parse_typevar_constraints(p: &mut Parser) {
    loop {
        parse_typevar_constraint(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Plus,
            ParseErrorContext::TypeVariableConstraintPlus,
            TYPEVAR_CONSTRAINT_FIRSTS,
        );
    }

    return;

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TYPEVAR_CONSTRAINT_FIRSTS) || p.at_end()
    }
}

fn parse_typevar_constraint(p: &mut Parser) {
    if p.at(TokenKind::Hash) {
        parse_typevar_constraint_kind_marker(p);
    } else if p.at(TokenKind::Ident) {
        parse_typevar_constraint_trait_marker(p);
    } else {
        p.error(ParseErrorContext::TypeVariableConstraint);
    }
}

const TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY: TokenSet = TYPEVAR_CONSTRAINT_FIRSTS
    .plus(TokenKind::LAngle)
    .plus(TokenKind::NilIdentifier)
    .plus(TokenKind::RAngle);

fn parse_typevar_constraint_kind_marker(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Hash));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::TypeKw,
        ParseErrorContext::TypeVariableKindConstraintTypeKw,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
    );

    if p.at_set(TokenSet::new([TokenKind::ClosedAngle])) {
        // this is a bit of a hack to allow better error reporting for `#Type<>`
        p.expect_with_recovery(
            TokenKind::NilIdentifier,
            ParseErrorContext::TypeVariableKindConstraintUnderscore,
            TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY
                // .union(TRAIT_RECOVERY_SET)
                .plus(TokenKind::ClosedAngle),
        );
        p.bump();

        return m.complete(p, SyntaxKind::TypeVariableKindConstraint);
    }

    p.expect_with_recovery(
        TokenKind::LAngle,
        ParseErrorContext::TypeVariableKindConstraintLAngle,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
    );
    p.expect_with_recovery(
        TokenKind::NilIdentifier,
        ParseErrorContext::TypeVariableKindConstraintUnderscore,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.plus(TokenKind::TypevarKw),
    );

    if p.at_set(TokenSet::new([TokenKind::Comma])) {
        p.bump();

        loop {
            if should_stop(p) {
                break;
            }

            p.expect_with_recovery(
                TokenKind::NilIdentifier,
                ParseErrorContext::TypeVariableKindConstraintUnderscore,
                TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
            );

            if should_stop(p) {
                break;
            }

            p.expect_with_recovery(
                TokenKind::Comma,
                ParseErrorContext::TypeVariableKindConstraintUnderscoreComma,
                TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
            );
        }
    }

    p.expect_with_recovery(
        TokenKind::RAngle,
        ParseErrorContext::TypeVariableKindConstraintRAngle,
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.plus(TokenKind::TypevarKw),
    );

    return m.complete(p, SyntaxKind::TypeVariableKindConstraint);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(TokenSet::new([TokenKind::RAngle])) || p.at_end()
    }
}

fn parse_typevar_constraint_trait_marker(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();

    m.complete(p, SyntaxKind::TypeVariableTraitConstraint)
}