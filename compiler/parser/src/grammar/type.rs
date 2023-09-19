#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(crate) enum ParseMode {
    InsideSelfContext,
    OutsideSelfContext,
}

pub(crate) fn parse_type(
    p: &mut Parser,
    context: ParseErrorContext,
    mode: ParseMode,
    single_type_recovery_set: TokenSet,
    parent_recovery_set: TokenSet,
) -> Option<CompletedMarker> {
    let single_type_m = match parse_single_type(p, context, mode, parent_recovery_set) {
        Some(m) => m,
        None => {
            p.error_with_recovery(context, single_type_recovery_set);
            return None;
        }
    };

    let cm = maybe_parse_bounded_type(
        p,
        mode,
        single_type_m,
        single_type_recovery_set,
        parent_recovery_set,
    );

    let cm = maybe_parse_lambda_type(
        p,
        context,
        mode,
        cm,
        single_type_recovery_set,
        parent_recovery_set,
    );

    Some(cm)
}

pub(crate) const SINGLE_TYPE_RECOVERY_SET: TokenSet =
    ts![TokenKind::Ident, TokenKind::SelfKw, TokenKind::LParen];

fn parse_single_type(
    p: &mut Parser,
    context: ParseErrorContext,
    mode: ParseMode,
    parent_recovery_set: TokenSet,
) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        Some(path::parse_path(
            p,
            context,
            ts![],
            SyntaxKind::TypeIdentifier,
        ))
    } else if mode == ParseMode::InsideSelfContext && p.at(TokenKind::SelfKw) {
        let m = p.start();
        p.bump();

        Some(m.complete(p, SyntaxKind::SelfType))
    } else if mode == ParseMode::OutsideSelfContext && p.maybe_at(TokenKind::SelfKw) {
        p.error_with_recovery(
            ParseErrorContext::SelfTypeOutsideContext,
            ts![TokenKind::SelfKw],
        );
        let m = p.start();
        p.bump();
        let cm = m.complete(p, SyntaxKind::SelfType);

        Some(cm)
    } else if p.at(TokenKind::LParen) {
        Some(parse_parenthesized_type(
            p,
            context,
            mode,
            parent_recovery_set,
        ))
    } else {
        None
    }
}

fn parse_parenthesized_type(
    p: &mut Parser,
    context: ParseErrorContext,
    mode: ParseMode,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::RParen) {
        p.bump();
        return m.complete(p, SyntaxKind::UnitType);
    }

    if p.at_set(parent_recovery_set) || p.at_top_level_token() {
        p.error_with_recovery(ParseErrorContext::UnitTypeRightParen, parent_recovery_set);
        return m.complete(p, SyntaxKind::UnitType);
    }

    parse_type(p, context, mode, ts![], parent_recovery_set);

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

            parse_type(p, context, mode, ts![], parent_recovery_set);

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
        !p.at_set(ts![TokenKind::Comma]) || p.at_eof()
    }
}

fn maybe_parse_bounded_type(
    p: &mut Parser,
    mode: ParseMode,
    cm: CompletedMarker,
    single_type_recovery_set: TokenSet,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    if !p.maybe_at(TokenKind::LAngle) {
        // if this isn't explicitly a bounded type, guess based on the next token
        if !p.at_set(SINGLE_TYPE_RECOVERY_SET) || p.at_set(single_type_recovery_set) {
            return cm;
        }
        if !p.maybe_at_nth(TokenKind::RAngle, 1) {
            return cm;
        }
    }

    let cm = cm.precede(p).complete(p, SyntaxKind::BoundedTypeBase);
    parse_bounded_type_args(p, mode, parent_recovery_set);

    cm.precede(p).complete(p, SyntaxKind::BoundedType)
}

fn maybe_parse_lambda_type(
    p: &mut Parser,
    context: ParseErrorContext,
    mode: ParseMode,
    cm: CompletedMarker,
    single_type_recovery_set: TokenSet,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    if !p.maybe_at(TokenKind::RightArrow) {
        // if this isn't explicitly a lambda type, guess based on the next token
        if !p.at_set(SINGLE_TYPE_RECOVERY_SET) || p.at_set(single_type_recovery_set) {
            return cm;
        }
    }

    p.expect_with_recovery(
        TokenKind::RightArrow,
        ParseErrorContext::LambdaTypeRightArrow,
        SINGLE_TYPE_RECOVERY_SET,
    );

    let m = cm.precede(p);

    parse_type(p, context, mode, ts![], parent_recovery_set);

    m.complete(p, SyntaxKind::LambdaType)
}

fn parse_bounded_type_args(p: &mut Parser, mode: ParseMode, parent_recovery_set: TokenSet) {
    p.expect_with_recovery(
        TokenKind::LAngle,
        ParseErrorContext::BoundedTypeLAngle,
        SINGLE_TYPE_RECOVERY_SET,
    );

    loop {
        if should_stop(p) {
            break;
        }

        let m = p.start();
        parse_type(
            p,
            ParseErrorContext::BoundedTypeArgType,
            mode,
            ts![TokenKind::Comma, TokenKind::Ident],
            parent_recovery_set,
        );
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
        p.at_set(ts![TokenKind::RAngle, TokenKind::WhereKw]) || p.at_top_level_token() || p.at_eof()
    }
}
