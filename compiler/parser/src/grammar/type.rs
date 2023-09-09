#[allow(clippy::wildcard_imports)]
use super::*;

pub(crate) fn parse_type_annotation(
    p: &mut Parser,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    assert!(p.at(TokenKind::TypeOfKw));

    let m = p.start();
    p.bump();

    ident::parse_ident_or_op(p, ParseErrorContext::TypeOfName, ts![TokenKind::Colon]);
    p.expect_with_recovery(
        TokenKind::Colon,
        ParseErrorContext::TypeOfColon,
        SINGLE_TYPE_RECOVERY_SET,
    );

    parse_type(p, TokenSet::EMPTY, parent_recovery_set);

    if p.at(TokenKind::WhereKw) {
        parse_type_annotation_type_variables(p, parent_recovery_set);
    }

    m.complete(p, SyntaxKind::TypeAnnotation)
}

fn parse_type_annotation_type_variables(p: &mut Parser, parent_recovery_set: TokenSet) {
    assert!(p.at(TokenKind::WhereKw));
    p.bump();

    loop {
        if should_stop(p) {
            break;
        }

        parse_generic_type_variable(p, parent_recovery_set);
    }

    return;

    fn should_stop(p: &mut Parser) -> bool {
        !p.maybe_at(TokenKind::TypevarKw) || p.at_end()
    }
}

fn parse_generic_type_variable(p: &mut Parser, parent_recovery_set: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::TypevarKw));

    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        parent_recovery_set,
    );

    if p.maybe_at(TokenKind::Equals) {
        p.bump();

        type_variable::parse_typevar_constraints(p);
    }

    m.complete(p, SyntaxKind::TypeVariable)
}

pub(crate) fn parse_type(
    p: &mut Parser,
    single_type_recovery_set: TokenSet,
    parent_recovery_set: TokenSet,
) -> Option<CompletedMarker> {
    let single_type_m = match parse_single_type(p, parent_recovery_set) {
        Some(m) => m,
        None => {
            p.error_with_recovery(ParseErrorContext::SingleType, single_type_recovery_set);
            return None;
        }
    };

    let single_type_m = maybe_parse_bounded_type(p, single_type_m, parent_recovery_set);

    if p.at(TokenKind::RightArrow) {
        let m = single_type_m.precede(p);

        p.bump();
        parse_type(p, TokenSet::EMPTY, parent_recovery_set);

        return Some(m.complete(p, SyntaxKind::LambdaType));
    }

    Some(single_type_m)
}

const SINGLE_TYPE_RECOVERY_SET: TokenSet =
    TokenSet::new([TokenKind::Ident, TokenKind::SelfKw, TokenKind::LParen]);

fn parse_single_type(p: &mut Parser, parent_recovery_set: TokenSet) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        Some(path::parse_path(
            p,
            ParseErrorContext::SingleType,
            TokenSet::EMPTY,
            SyntaxKind::TypeIdentifier,
        ))
    } else if p.at(TokenKind::SelfKw) {
        // TODO: don't accept Self when parsing outside a module
        // or maybe catch this later, like when translating to the AST
        let m = p.start();
        p.bump();

        Some(m.complete(p, SyntaxKind::SelfType))
    } else if p.at(TokenKind::LParen) {
        Some(parse_parenthesized_type(p, parent_recovery_set))
    } else {
        None
    }
}

fn parse_parenthesized_type(p: &mut Parser, parent_recovery_set: TokenSet) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::UnitType);
    }

    let local_parent_recovery_set = DEFAULT_RECOVERY_SET.union(parent_recovery_set);
    if p.at_set(local_parent_recovery_set) {
        p.error_with_recovery(
            ParseErrorContext::UnitTypeRightParen,
            local_parent_recovery_set,
        );
        return m.complete(p, SyntaxKind::UnitType);
    }

    parse_type(p, TokenSet::EMPTY, parent_recovery_set);

    let mut comma_count = 0;
    let mut arg_count = 1;
    if p.at(TokenKind::Comma) {
        loop {
            if should_stop(p) {
                break;
            }

            comma_count += 1;

            if p.at(TokenKind::Comma) {
                p.bump();
            }

            parse_type(p, TokenSet::EMPTY, parent_recovery_set);

            arg_count += 1;
        }
    }

    let recovery_context = match arg_count {
        1 => ParseErrorContext::ParenthesizedTypeRightParen,
        _ => ParseErrorContext::TupleTypeRightParen,
    };

    p.expect_with_recovery(TokenKind::RParen, recovery_context, parent_recovery_set);

    return if comma_count == 0 {
        m.complete(p, SyntaxKind::ParenthesizedType)
    } else {
        m.complete(p, SyntaxKind::TupleType)
    };

    fn should_stop(p: &mut Parser) -> bool {
        !p.at_set(ts![TokenKind::Comma]) || p.at_end()
    }
}

const ACCEPTABLE_BOUNDED_TYPE_FIRSTS: TokenSet = TokenSet::new([TokenKind::LAngle]);

fn maybe_parse_bounded_type(
    p: &mut Parser,
    cm: CompletedMarker,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    if !(p.at_set(ACCEPTABLE_BOUNDED_TYPE_FIRSTS)) {
        return cm;
    }

    let cm = cm.precede(p).complete(p, SyntaxKind::BoundedTypeBase);
    parse_bounded_type_args(p, parent_recovery_set);

    cm.precede(p).complete(p, SyntaxKind::BoundedType)
}

fn parse_bounded_type_args(p: &mut Parser, parent_recovery_set: TokenSet) {
    p.expect_with_recovery(
        TokenKind::LAngle,
        ParseErrorContext::BoundedTypeLAngle,
        ts![TokenKind::Ident],
    );

    loop {
        if should_stop(p) {
            break;
        }

        let m = p.start();
        parse_type(p, ts![TokenKind::Comma], parent_recovery_set);
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
        ts![TokenKind::WhereKw],
    );

    return;

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(ts![TokenKind::RAngle, TokenKind::WhereKw]) || p.at_end()
    }
}
