use super::*;

use crate::grammar::lambda;

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
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

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    parse_expr_with_binding_power(p, 0)
}

fn parse_expr_with_binding_power(
    p: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        let op = if p.at(TokenKind::Plus) {
            BinaryOp::Add
        } else if p.at(TokenKind::Minus) {
            BinaryOp::Sub
        } else if p.at(TokenKind::Star) {
            BinaryOp::Mul
        } else if p.at(TokenKind::Slash) {
            BinaryOp::Div
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
        let parsed_rhs = parse_expr_with_binding_power(p, right_binding_power).is_some();
        lhs = m.complete(p, SyntaxKind::InfixExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
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
        p.error();
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

fn parse_fractional_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Fractional));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FractionalLiteral)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::String));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteral)
}

fn parse_char_literal(p: &mut Parser) -> CompletedMarker {
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
    parse_expr(p);
    if_m.complete(p, SyntaxKind::IfExpr);

    p.expect(TokenKind::ThenKw);
    let then_m = p.start();
    parse_expr(p);
    then_m.complete(p, SyntaxKind::ThenExpr);

    p.expect(TokenKind::ElseKw);
    let else_m = p.start();
    parse_expr(p);
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

    parse_expr_with_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::PrefixExpr)
}

fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();
    parse_expr(p);
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_integer() {
        check(
            "123",
            expect![[r#"
Root@0..3
  IntLiteral@0..3
    Integer@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_fractional() {
        check(
            "123.123",
            expect![[r#"
Root@0..7
  FractionalLiteral@0..7
    Fractional@0..7 "123.123""#]],
        );
    }

    #[test]
    fn parse_string() {
        check(
            r#""hello""#,
            expect![[r#"
Root@0..7
  StringLiteral@0..7
    String@0..7 "\"hello\"""#]],
        );
    }

    #[test]
    fn parse_char() {
        check(
            "'c'",
            expect![[r#"
Root@0..3
  CharLiteral@0..3
    Char@0..3 "'c'""#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
Root@0..7
  VariableRef@0..7
    Ident@0..7 "counter""#]],
        );
    }

    #[test]
    fn parse_simple_infix_expression() {
        check(
            "1 +2",
            expect![[r#"
Root@0..4
  InfixExpr@0..4
    IntLiteral@0..2
      Integer@0..1 "1"
      Whitespace@1..2 " "
    Plus@2..3 "+"
    IntLiteral@3..4
      Integer@3..4 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_infix_expression() {
        check(
            "1 +  2  +  3 +4",
            expect![[r#"
Root@0..15
  InfixExpr@0..15
    InfixExpr@0..13
      InfixExpr@0..8
        IntLiteral@0..2
          Integer@0..1 "1"
          Whitespace@1..2 " "
        Plus@2..3 "+"
        Whitespace@3..5 "  "
        IntLiteral@5..8
          Integer@5..6 "2"
          Whitespace@6..8 "  "
      Plus@8..9 "+"
      Whitespace@9..11 "  "
      IntLiteral@11..13
        Integer@11..12 "3"
        Whitespace@12..13 " "
    Plus@13..14 "+"
    IntLiteral@14..15
      Integer@14..15 "4""#]],
        );
    }

    #[test]
    fn parse_infix_expression_with_mixed_binding_power() {
        check(
            "1 + 2 * 3 - 4",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    InfixExpr@0..10
      IntLiteral@0..2
        Integer@0..1 "1"
        Whitespace@1..2 " "
      Plus@2..3 "+"
      Whitespace@3..4 " "
      InfixExpr@4..10
        IntLiteral@4..6
          Integer@4..5 "2"
          Whitespace@5..6 " "
        Star@6..7 "*"
        Whitespace@7..8 " "
        IntLiteral@8..10
          Integer@8..9 "3"
          Whitespace@9..10 " "
    Minus@10..11 "-"
    Whitespace@11..12 " "
    IntLiteral@12..13
      Integer@12..13 "4""#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "- 10",
            expect![[r#"
Root@0..4
  PrefixExpr@0..4
    Minus@0..1 "-"
    Whitespace@1..2 " "
    IntLiteral@2..4
      Integer@2..4 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20 + 20",
            expect![[r#"
Root@0..8
  InfixExpr@0..8
    PrefixExpr@0..4
      Minus@0..1 "-"
      IntLiteral@1..4
        Integer@1..3 "20"
        Whitespace@3..4 " "
    Plus@4..5 "+"
    Whitespace@5..6 " "
    IntLiteral@6..8
      Integer@6..8 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "(((((( 10)) ))))",
            expect![[r#"
Root@0..16
  ParenExpr@0..16
    LParen@0..1 "("
    ParenExpr@1..15
      LParen@1..2 "("
      ParenExpr@2..14
        LParen@2..3 "("
        ParenExpr@3..13
          LParen@3..4 "("
          ParenExpr@4..12
            LParen@4..5 "("
            ParenExpr@5..10
              LParen@5..6 "("
              Whitespace@6..7 " "
              IntLiteral@7..9
                Integer@7..9 "10"
              RParen@9..10 ")"
            RParen@10..11 ")"
            Whitespace@11..12 " "
          RParen@12..13 ")"
        RParen@13..14 ")"
      RParen@14..15 ")"
    RParen@15..16 ")""#]],
        );
    }

    #[test]
    fn parentheses_affect_precedence() {
        check(
            "5 *  ( 2 + 1)",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    IntLiteral@0..2
      Integer@0..1 "5"
      Whitespace@1..2 " "
    Star@2..3 "*"
    Whitespace@3..5 "  "
    ParenExpr@5..13
      LParen@5..6 "("
      Whitespace@6..7 " "
      InfixExpr@7..12
        IntLiteral@7..9
          Integer@7..8 "2"
          Whitespace@8..9 " "
        Plus@9..10 "+"
        Whitespace@10..11 " "
        IntLiteral@11..12
          Integer@11..12 "1"
      RParen@12..13 ")""#]],
        );
    }

    #[test]
    fn parse_number_preceded_by_whitespace() {
        check(
            "   9876",
            expect![[r#"
Root@0..7
  Whitespace@0..3 "   "
  IntLiteral@3..7
    Integer@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_number_followed_by_whitespace() {
        check(
            "999   ",
            expect![[r#"
Root@0..6
  IntLiteral@0..6
    Integer@0..3 "999"
    Whitespace@3..6 "   ""#]],
        );
    }

    #[test]
    fn parse_number_surrounded_by_whitespace() {
        check(
            " 123     ",
            expect![[r#"
Root@0..9
  Whitespace@0..1 " "
  IntLiteral@1..9
    Integer@1..4 "123"
    Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_infix_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 -- Add one
  + 10 -- Add ten",
            expect![[r##"
Root@0..37
  Whitespace@0..1 "\n"
  InfixExpr@1..37
    InfixExpr@1..22
      IntLiteral@1..5
        Integer@1..2 "1"
        Whitespace@2..5 "\n  "
      Plus@5..6 "+"
      Whitespace@6..7 " "
      IntLiteral@7..22
        Integer@7..8 "1"
        Whitespace@8..9 " "
        Comment@9..19 "-- Add one"
        Whitespace@19..22 "\n  "
    Plus@22..23 "+"
    Whitespace@23..24 " "
    IntLiteral@24..37
      Integer@24..26 "10"
      Whitespace@26..27 " "
      Comment@27..37 "-- Add ten""##]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(foo",
            expect![[r#"
                Root@0..4
                  ParenExpr@0..4
                    LParen@0..1 "("
                    VariableRef@1..4
                      Ident@1..4 "foo"
                error at 1..4: expected ‘+’, ‘-’, ‘*’, ‘/’ or ‘)’"#]],
        );
    }

    #[test]
    fn do_not_parse_operator_if_getting_rhs_failed() {
        check(
            "(1+",
            expect![[r#"
                Root@0..3
                  ParenExpr@0..3
                    LParen@0..1 "("
                    InfixExpr@1..3
                      IntLiteral@1..2
                        Integer@1..2 "1"
                      Plus@2..3 "+"
                error at 2..3: expected integer, fractional, string, char, identifier, ‘-’, ‘(’, ‘if‘ or ‘|’
                error at 2..3: expected ‘)’"#]],
        );
    }

    #[test]
    fn parse_if_then_else() {
        check(
            "if test then 2 else 3",
            expect![[r#"
                Root@0..21
                  IfThenElseExpr@0..21
                    IfKw@0..2 "if"
                    Whitespace@2..3 " "
                    IfExpr@3..8
                      VariableRef@3..8
                        Ident@3..7 "test"
                        Whitespace@7..8 " "
                    ThenKw@8..12 "then"
                    Whitespace@12..13 " "
                    ThenExpr@13..15
                      IntLiteral@13..15
                        Integer@13..14 "2"
                        Whitespace@14..15 " "
                    ElseKw@15..19 "else"
                    Whitespace@19..20 " "
                    ElseExpr@20..21
                      IntLiteral@20..21
                        Integer@20..21 "3""#]],
        );
    }
}
