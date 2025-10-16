#[allow(clippy::wildcard_imports)]
use super::*;

use crate::grammar::argument::{parse_argument, ARGUMENT_RECOVERY_SET};
use crate::grammar::lambda;

pub(crate) const EXPR_FIRSTS: TokenSet = ts![
    TokenKind::Integer,
    TokenKind::Fraction,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::Ident,
    TokenKind::Minus,
    TokenKind::LParen,
    TokenKind::IfKw,
    TokenKind::Pipe,
];

enum BinaryOp {
    Custom,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Custom => (1, 2),
            Self::Add | Self::Sub => (3, 4),
            Self::Mul | Self::Div => (5, 6),
        }
    }
}

enum UnaryOp {
    Neg,
}

impl UnaryOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 7),
        }
    }
}

pub(super) fn parse_expr(p: &mut Parser, context: ParseErrorContext) -> Option<CompletedMarker> {
    parse_expr_with_recovery(p, ts![], context)
}

fn parse_expr_with_recovery(
    p: &mut Parser,
    recovery_set: TokenSet,
    context: ParseErrorContext,
) -> Option<CompletedMarker> {
    parse_expr_with_binding_power(p, 0, recovery_set, context)
}

const SUPPORTED_OPERATORS: TokenSet = ts![
    TokenKind::Plus,
    TokenKind::Minus,
    TokenKind::Star,
    TokenKind::Slash,
    TokenKind::OpIdent,
];

fn parse_expr_with_binding_power(
    p: &mut Parser,
    minimum_binding_power: u8,
    recovery_set: TokenSet,
    context: ParseErrorContext,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p, recovery_set, context)?;

    loop {
        if !p.at_set(SUPPORTED_OPERATORS) {
            // We’re not at an operator we recognize;
            // we don’t know what to do next, so we return and let caller decide.
            break;
        }

        let op = if p.at(TokenKind::Plus) {
            BinaryOp::Add
        } else if p.at(TokenKind::Minus) {
            BinaryOp::Sub
        } else if p.at(TokenKind::Star) {
            BinaryOp::Mul
        } else if p.at(TokenKind::Slash) {
            BinaryOp::Div
        } else if p.at(TokenKind::OpIdent) {
            BinaryOp::Custom
        } else {
            unreachable!("we should never end up here, since the list of known operators is checked beforehand")
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Eat the operator’s token.
        p.bump_any();

        let m = lhs.precede(p);
        let parsed_rhs =
            parse_expr_with_binding_power(p, right_binding_power, recovery_set, context).is_some();
        lhs = m.complete(p, SyntaxKind::InfixExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn parse_lhs(
    p: &mut Parser,
    recovery_set: TokenSet,
    context: ParseErrorContext,
) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Integer) {
        parse_int_literal(p)
    } else if p.at(TokenKind::Fraction) {
        parse_fraction_literal(p)
    } else if p.at(TokenKind::String) {
        parse_string_literal(p)
    } else if p.at(TokenKind::Char) {
        parse_char_literal(p)
    } else if p.at(TokenKind::Ident) || p.at(TokenKind::OpIdent) {
        parse_variable_ref(p)
    } else if p.at(TokenKind::Minus) {
        parse_prefix_expr(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p)
    } else if p.at(TokenKind::IfKw) {
        parse_if_then_else_expr(p)
    } else if p.at(TokenKind::MatchKw) {
        parse_match_when_expr(p)
    } else if p.at(TokenKind::Pipe) {
        lambda::parse_lambda_expr(p)
    } else {
        p.error_with_recovery(context, recovery_set);
        return None;
    };

    Some(cm)
}

pub(crate) fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::Integer);
    m.complete(p, SyntaxKind::IntLiteral)
}

pub(crate) fn parse_fraction_literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::Fraction);
    m.complete(p, SyntaxKind::FractionLiteral)
}

pub(crate) fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::String);
    m.complete(p, SyntaxKind::StringLiteral)
}

pub(crate) fn parse_char_literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::Char);
    m.complete(p, SyntaxKind::CharLiteral)
}

pub(crate) fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    let cm = path::parse_path(
        p,
        ParseErrorContext::VariableRef,
        ts![],
        SyntaxKind::VariableRef,
    );

    maybe_parse_function_call(p, cm)
}

fn maybe_parse_function_call(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    if !p.maybe_at(TokenKind::LParen) {
        return lhs;
    }

    parse_function_call(p);

    lhs.precede(p).complete(p, SyntaxKind::FunctionCall)
}

fn parse_function_call(p: &mut Parser) -> CompletedMarker {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_set(ts![TokenKind::RParen])
    }

    let paren_m = p.start();
    p.bump(TokenKind::LParen);

    loop {
        if should_stop(p) {
            break;
        }

        parse_expr_with_recovery(
            p,
            ts![TokenKind::RParen, TokenKind::Comma],
            ParseErrorContext::FunctionCallArgExpr,
        );

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::FunctionCallArgComma,
            EXPR_FIRSTS,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::FunctionCallRightParen);

    paren_m.complete(p, SyntaxKind::FunctionCallArgList)
}

fn parse_if_then_else_expr(p: &mut Parser) -> CompletedMarker {
    let if_then_else_m = p.start();
    p.bump(TokenKind::IfKw);

    let if_m = p.start();
    parse_expr_with_recovery(
        p,
        ts![TokenKind::ThenKw],
        ParseErrorContext::IfThenElseIfExpr,
    );
    if_m.complete(p, SyntaxKind::IfExpr);

    p.expect_with_recovery(
        TokenKind::ThenKw,
        ParseErrorContext::IfThenElseThenKw,
        EXPR_FIRSTS,
    );
    let then_m = p.start();
    parse_expr_with_recovery(
        p,
        ts![TokenKind::ElseKw],
        ParseErrorContext::IfThenElseThenExpr,
    );
    then_m.complete(p, SyntaxKind::ThenExpr);

    p.expect_with_recovery(
        TokenKind::ElseKw,
        ParseErrorContext::IfThenElseElseKw,
        EXPR_FIRSTS,
    );
    let else_m = p.start();
    parse_expr(p, ParseErrorContext::IfThenElseElseExpr);
    else_m.complete(p, SyntaxKind::ElseExpr);

    if_then_else_m.complete(p, SyntaxKind::IfThenElseExpr)
}

fn parse_match_when_expr(p: &mut Parser) -> CompletedMarker {
    let match_when_m = p.start();
    p.bump(TokenKind::MatchKw);

    let match_m = p.start();
    parse_expr_with_recovery(p, ts![TokenKind::WhenKw], ParseErrorContext::MatchExprArg);
    match_m.complete(p, SyntaxKind::MatchExprArg);

    p.expect_with_recovery(
        TokenKind::WhenKw,
        ParseErrorContext::MatchExprWhenKw,
        ts![TokenKind::Backslash],
    );

    loop {
        let when_m = p.start();
        p.expect_with_recovery(
            TokenKind::Backslash,
            ParseErrorContext::MatchTargetPipe,
            ARGUMENT_RECOVERY_SET,
        );

        parse_argument(
            p,
            SyntaxKind::MatchTargetCondition,
            ParseErrorContext::MatchTargetCondition,
            ts![TokenKind::RightArrow],
            false,
        );

        p.expect_with_recovery(
            TokenKind::RightArrow,
            ParseErrorContext::MatchTargetRightArrow,
            EXPR_FIRSTS,
        );

        let when_value_m = p.start();
        parse_expr_with_recovery(
            p,
            ts![TokenKind::Backslash],
            ParseErrorContext::MatchTargetValue,
        );
        when_value_m.complete(p, SyntaxKind::MatchTargetValue);

        when_m.complete(p, SyntaxKind::MatchTarget);

        if p.at_top_level_token_or_not_set(ts![TokenKind::Backslash]) {
            break;
        }
    }

    match_when_m.complete(p, SyntaxKind::MatchExpr)
}

fn parse_prefix_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Eat the operator’s token.
    p.bump(TokenKind::Minus);

    parse_expr_with_binding_power(
        p,
        right_binding_power,
        TokenSet::default(),
        ParseErrorContext::PrefixExprExpr,
    );

    m.complete(p, SyntaxKind::UnaryExpr)
}

fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
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

        parse_expr_with_recovery(p, ts![TokenKind::RParen], ParseErrorContext::ParenExprExpr);
        arg_len += 1;

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ParenExprComma,
            EXPR_FIRSTS,
        );
    }

    p.expect(TokenKind::RParen, ParseErrorContext::ParenExprRightParen);

    let kind = match arg_len {
        0 => SyntaxKind::Unit,
        1 => SyntaxKind::ParenExpr,
        _ => SyntaxKind::TupleExpr,
    };
    paren_m.complete(p, kind)
}
