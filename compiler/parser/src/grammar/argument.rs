#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::expr::{
    parse_char_literal, parse_fraction_literal, parse_int_literal, parse_string_literal,
};

pub(crate) const ARGUMENT_RECOVERY_SET: TokenSet = ts![
    TokenKind::Integer,
    TokenKind::Fraction,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::Ident,
    TokenKind::LParen,
    TokenKind::NilIdentifier,
];

pub(crate) fn parse_argument(
    p: &mut Parser,
    kind: SyntaxKind,
    context: ParseErrorContext,
    recovery_set: TokenSet,
) -> CompletedMarker {
    let m = p.start();

    if p.at(TokenKind::Integer) {
        parse_int_literal(p);
    } else if p.at(TokenKind::Fraction) {
        parse_fraction_literal(p);
    } else if p.at(TokenKind::String) {
        parse_string_literal(p);
    } else if p.at(TokenKind::Char) {
        parse_char_literal(p);
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p);
    } else if p.at(TokenKind::LParen) {
        parse_tuple_arg(p);
    } else if p.at(TokenKind::NilIdentifier) {
        p.bump(TokenKind::NilIdentifier);
    } else {
        p.error_with_recovery(context, recovery_set);
    }

    m.complete(p, kind)
}

fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::Ident);

    let cm = if !(p.maybe_at(TokenKind::DoubleColon)) {
        m.complete(p, SyntaxKind::VariableRef)
    } else {
        m.cancel(p);

        path::parse_path(
            p,
            ParseErrorContext::VariableRef,
            ts![],
            SyntaxKind::VariableRef,
        )
    };

    maybe_parse_typedef_destructuring(p, cm)
}

fn maybe_parse_typedef_destructuring(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    if !p.maybe_at(TokenKind::LParen) {
        return lhs;
    }

    let cm = lhs.precede(p).complete(p, SyntaxKind::DestructorTarget);
    parse_typedef_destructuring(p);

    cm.precede(p).complete(p, SyntaxKind::Destructor)
}

fn parse_typedef_destructuring(p: &mut Parser) -> CompletedMarker {
    let paren_m = p.start();
    p.bump(TokenKind::LParen);

    loop {
        if should_stop(p) {
            break;
        }

        parse_argument(
            p,
            SyntaxKind::DestructorArg,
            ParseErrorContext::DestructorArgPattern,
            ts![TokenKind::RParen, TokenKind::Comma],
        );

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::DestructorArgComma,
            ARGUMENT_RECOVERY_SET,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::DestructorRightParen);

    return paren_m.complete(p, SyntaxKind::DestructorArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.maybe_at(TokenKind::RParen) || p.at_top_level_token() || p.at_eof()
    }
}

fn parse_tuple_arg(p: &mut Parser) -> CompletedMarker {
    let paren_m = p.start();
    p.bump(TokenKind::LParen);

    let mut arg_len = 0;
    loop {
        if should_stop(p) {
            break;
        }

        parse_argument(
            p,
            SyntaxKind::TupleExprArg,
            ParseErrorContext::ParenExprExpr,
            ts![TokenKind::RightArrow],
        );
        arg_len += 1;

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ParenExprComma,
            ARGUMENT_RECOVERY_SET,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::ParenExprRightParen);

    let kind = match arg_len {
        0 => SyntaxKind::UnitExpr,
        1 => SyntaxKind::ParenExpr,
        _ => SyntaxKind::TupleExpr,
    };
    return paren_m.complete(p, kind);

    fn should_stop(p: &mut Parser) -> bool {
        p.maybe_at(TokenKind::RParen) || p.at_eof()
    }
}
