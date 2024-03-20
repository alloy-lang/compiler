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
    identify_declarations: bool,
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
    } else if identify_declarations
        && p.at(TokenKind::Ident)
        && !p.maybe_at_nth(TokenKind::DoubleColon, 1)
    {
        parse_variable_declaration(p);
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p);
    } else if p.at(TokenKind::Minus) {
        parse_prefix_expr(p, recovery_set);
    } else if p.at(TokenKind::LParen) {
        parse_tuple_arg(p, identify_declarations);
    } else if p.at(TokenKind::NilIdentifier) {
        p.bump(TokenKind::NilIdentifier);
    } else {
        p.error_with_recovery(context, recovery_set);
    }

    m.complete(p, kind)
}

fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    let cm = path::parse_path(
        p,
        ParseErrorContext::VariableRef,
        ts![],
        SyntaxKind::VariableRef,
    );

    maybe_parse_typedef_destructure(p, cm)
}

fn parse_variable_declaration(p: &mut Parser) -> CompletedMarker {
    let parent_m = p.start();
    p.expect(TokenKind::Ident, ParseErrorContext::VariableDeclaration);
    parent_m.complete(p, SyntaxKind::VariableDeclaration)
}

fn maybe_parse_typedef_destructure(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    if !p.maybe_at(TokenKind::LParen) {
        return lhs;
    }

    let cm = lhs.precede(p).complete(p, SyntaxKind::DestructureTarget);
    parse_typedef_destructure_args(p);

    cm.precede(p).complete(p, SyntaxKind::Destructure)
}

fn parse_typedef_destructure_args(p: &mut Parser) {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_set(ts![TokenKind::RParen])
    }

    p.bump(TokenKind::LParen);

    loop {
        if should_stop(p) {
            break;
        }

        parse_argument(
            p,
            SyntaxKind::DestructureArg,
            ParseErrorContext::DestructureArgPattern,
            ts![TokenKind::RParen, TokenKind::Comma],
            true,
        );

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::DestructureArgComma,
            ARGUMENT_RECOVERY_SET,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::DestructureRightParen);
}

fn parse_tuple_arg(p: &mut Parser, identify_declarations: bool) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_set(ts![TokenKind::RParen])
    }

    let paren_m = p.start();
    p.bump(TokenKind::LParen);

    let mut arg_len = 0;
    loop {
        if should_stop(p) {
            break;
        }

        parse_argument(
            p,
            SyntaxKind::TuplePatternArg,
            ParseErrorContext::ParenExprExpr,
            ts![TokenKind::RightArrow],
            identify_declarations,
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
        0 => SyntaxKind::Unit,
        1 => SyntaxKind::ParenPattern,
        _ => SyntaxKind::TuplePattern,
    };
    paren_m.complete(p, kind)
}

fn parse_prefix_expr(p: &mut Parser, parent_recovery_set: TokenSet) -> CompletedMarker {
    let m = p.start();

    // Eat the operator’s token.
    p.bump(TokenKind::Minus);

    if p.at(TokenKind::Integer) {
        p.bump(TokenKind::Integer);
    } else {
        p.expect_with_recovery(
            TokenKind::Fraction,
            ParseErrorContext::PrefixPatternPattern,
            parent_recovery_set,
        );
    }

    m.complete(p, SyntaxKind::UnaryExpr)
}
