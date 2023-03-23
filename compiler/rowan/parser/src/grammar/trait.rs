use super::*;
use crate::parser::DEFAULT_RECOVERY_SET;

const TRAIT_RECOVERY_SET: TokenSet =
    TokenSet::new([TokenKind::TypevarKw, TokenKind::TypeOfKw, TokenKind::EndKw]);

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
// Type Annotation
//

fn parse_trait_type_annotation(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::TypeOfKw));

    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident, ParseErrorContext::TypeOfName);
    p.expect(TokenKind::Colon, ParseErrorContext::TypeOfColon);

    parse_type(p, TokenSet::EMPTY);

    if p.at(TokenKind::WhereKw) {
        parse_trait_type_annotation_type_variables(p);
    }

    m.complete(p, SyntaxKind::TypeAnnotation)
}

fn parse_trait_type_annotation_type_variables(p: &mut Parser) {
    assert!(p.at(TokenKind::WhereKw));
    p.bump();

    loop {
        if should_stop(p) {
            break;
        }

        parse_trait_type_variable(p);
    }

    return;

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::TypevarKw])) || p.at_end()
    }
}

fn parse_type(p: &mut Parser, recovery_set: TokenSet) -> Option<CompletedMarker> {
    let single_type_m = match parse_single_type(p) {
        Some(m) => m,
        None => {
            p.error_with_recovery(ParseErrorContext::SingleType, recovery_set);
            return None;
        }
    };

    let single_type_m = maybe_parse_bounded_type(p, single_type_m);

    if p.at(TokenKind::RightArrow) {
        let m = single_type_m.precede(p);

        p.bump();
        parse_type(p, TokenSet::EMPTY);

        return Some(m.complete(p, SyntaxKind::LambdaType));
    }

    Some(single_type_m)
}

const SINGLE_TYPE_RECOVERY_SET: TokenSet =
    TokenSet::new([TokenKind::Ident, TokenKind::SelfKw, TokenKind::LParen]);

fn parse_single_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump();

        Some(m.complete(p, SyntaxKind::TypeIdentifier))
    } else if p.at(TokenKind::SelfKw) {
        let m = p.start();
        p.bump();

        Some(m.complete(p, SyntaxKind::SelfType))
    } else if p.at(TokenKind::LParen) {
        Some(parse_parenthesized_type(p))
    } else {
        None
    }
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

    parse_type(p, TokenSet::EMPTY);

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

            parse_type(p, TokenSet::EMPTY);
        }
    }

    p.expect_with_recovery(
        TokenKind::RParen,
        ParseErrorContext::UnitTypeRightParen,
        TRAIT_RECOVERY_SET,
    );

    return if comma_count == 0 {
        m.complete(p, SyntaxKind::ParenthesizedType)
    } else {
        m.complete(p, SyntaxKind::TupleType)
    };

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(TokenSet::new([TokenKind::Comma])) || p.at_end()
    }
}

const ACCEPTABLE_BOUNDED_TYPE_FIRSTS: TokenSet = TokenSet::new([TokenKind::LAngle]);

fn maybe_parse_bounded_type(p: &mut Parser, cm: CompletedMarker) -> CompletedMarker {
    if !(p.at_set(ACCEPTABLE_BOUNDED_TYPE_FIRSTS)) {
        return cm;
    }

    let cm = cm.precede(p).complete(p, SyntaxKind::BoundedTypeBase);
    parse_bounded_type_args(p);

    cm.precede(p).complete(p, SyntaxKind::BoundedType)
}

fn parse_bounded_type_args(p: &mut Parser) {
    p.expect_with_recovery(
        TokenKind::LAngle,
        ParseErrorContext::BoundedTypeLAngle,
        TokenSet::new([TokenKind::Ident]),
    );

    loop {
        if should_stop(p) {
            break;
        }

        let m = p.start();
        parse_type(p, TokenSet::new([TokenKind::Comma]));
        m.complete(p, SyntaxKind::BoundedTypeArg);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::BoundedTypeComma,
            SINGLE_TYPE_RECOVERY_SET,
        );
    }

    p.expect_with_recovery(
        TokenKind::RAngle,
        ParseErrorContext::BoundedTypeRAngle,
        TRAIT_RECOVERY_SET.plus(TokenKind::WhereKw),
    );

    return;

    const RECOVERY_SET: TokenSet = TRAIT_RECOVERY_SET
        .plus(TokenKind::RAngle)
        .plus(TokenKind::WhereKw);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(RECOVERY_SET) || p.at_end()
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

    parse_typevar_constraints(p);

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

        parse_typevar_constraints(p);
    }

    m.complete(p, SyntaxKind::TypeVariable)
}

const TYPEVAR_CONSTRAINT_FIRSTS: TokenSet =
    TokenSet::new([TokenKind::Hash, TokenKind::Ident, TokenKind::Plus]);

fn parse_typevar_constraints(p: &mut Parser) {
    loop {
        parse_typevar_constraint(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Plus,
            ParseErrorContext::TypeVariableConstraintPlus,
            TRAIT_RECOVERY_SET.union(TYPEVAR_CONSTRAINT_FIRSTS),
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

const TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY: TokenSet = TRAIT_RECOVERY_SET
    .union(TYPEVAR_CONSTRAINT_FIRSTS)
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
            TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY.plus(TokenKind::ClosedAngle),
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
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
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
        TYPEVAR_CONSTRAINT_KIND_MARKER_RECOVERY,
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
