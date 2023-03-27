use super::*;

use crate::grammar::lambda;

const EXPR_RECOVERY_SET: TokenSet = TokenSet::new([
    TokenKind::Integer,
    TokenKind::Fractional,
    TokenKind::String,
    TokenKind::Char,
    TokenKind::Ident,
    TokenKind::Minus,
    TokenKind::LParen,
    TokenKind::IfKw,
    TokenKind::Pipe,
]);

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
            Self::Neg => ((), 5),
        }
    }
}

pub(super) fn parse_expr(p: &mut Parser, context: ParseErrorContext) -> Option<CompletedMarker> {
    parse_expr_with_recovery(p, TokenSet::default(), context)
}

fn parse_expr_with_recovery(
    p: &mut Parser,
    recovery_set: TokenSet,
    context: ParseErrorContext,
) -> Option<CompletedMarker> {
    parse_expr_with_binding_power(p, 0, recovery_set, context)
}

fn parse_expr_with_binding_power(
    p: &mut Parser,
    minimum_binding_power: u8,
    recovery_set: TokenSet,
    context: ParseErrorContext,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p, recovery_set, context)?;

    loop {
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
            // We’re not at an operator; we don’t know what to do next, so we return and let the
            // caller decide.
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Eat the operator’s token.
        p.bump();

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
    } else if p.at(TokenKind::Fractional) {
        parse_fractional_literal(p)
    } else if p.at(TokenKind::String) {
        parse_string_literal(p)
    } else if p.at(TokenKind::Char) {
        parse_char_literal(p)
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p)
    } else if p.at(TokenKind::Minus) {
        parse_prefix_expr(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p)
    } else if p.at(TokenKind::IfKw) {
        parse_if_then_else_expr(p)
    } else if p.at(TokenKind::Pipe) {
        lambda::parse_lambda_expr(p)
    } else {
        p.error_with_recovery(context, recovery_set);
        return None;
    };

    Some(cm)
}

pub(crate) fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Integer));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntLiteral)
}

pub(crate) fn parse_fractional_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Fractional));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FractionalLiteral)
}

pub(crate) fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::String));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteral)
}

pub(crate) fn parse_char_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Char));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::CharLiteral)
}

pub(crate) fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::VariableRef)
}

fn parse_if_then_else_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::IfKw));

    let if_then_else_m = p.start();
    p.bump();

    let if_m = p.start();
    parse_expr_with_recovery(
        p,
        TokenSet::new([TokenKind::ThenKw]),
        ParseErrorContext::IfThenElseIfExpr,
    );
    if_m.complete(p, SyntaxKind::IfExpr);

    p.expect_with_recovery(
        TokenKind::ThenKw,
        ParseErrorContext::IfThenElseThenKw,
        EXPR_RECOVERY_SET,
    );
    let then_m = p.start();
    parse_expr_with_recovery(
        p,
        TokenSet::new([TokenKind::ElseKw]),
        ParseErrorContext::IfThenElseThenExpr,
    );
    then_m.complete(p, SyntaxKind::ThenExpr);

    p.expect_with_recovery(
        TokenKind::ElseKw,
        ParseErrorContext::IfThenElseElseKw,
        EXPR_RECOVERY_SET,
    );
    let else_m = p.start();
    parse_expr(p, ParseErrorContext::IfThenElseElseExpr);
    else_m.complete(p, SyntaxKind::ElseExpr);

    if_then_else_m.complete(p, SyntaxKind::IfThenElseExpr)
}

fn parse_prefix_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Minus));

    let m = p.start();

    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Eat the operator’s token.
    p.bump();

    parse_expr_with_binding_power(
        p,
        right_binding_power,
        TokenSet::default(),
        ParseErrorContext::PrefixExprExpr,
    );

    m.complete(p, SyntaxKind::PrefixExpr)
}

fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let paren_m = p.start();
    p.bump();

    let mut arg_len = 0;
    loop {
        if should_stop(p) {
            break;
        }

        parse_expr(p, ParseErrorContext::ParenExprExpr);
        arg_len += 1;

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(
            TokenKind::Comma,
            ParseErrorContext::ParenExprComma,
            EXPR_RECOVERY_SET,
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
        p.at_set(TokenSet::new([TokenKind::RParen])) || p.at_end()
    }
}
