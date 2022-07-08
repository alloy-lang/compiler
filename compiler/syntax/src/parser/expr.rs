use super::Parser;
use crate::lexer::SyntaxKind;

enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

enum PrefixOp {
    Neg,
}

impl PrefixOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
        }
    }
}

pub(super) fn expr(p: &mut Parser) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
    let mut lhs = match p.peek() {
        Some(SyntaxKind::Number) => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::Literal)
        }
        Some(SyntaxKind::Ident) => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::VariableRef)
        }
        Some(SyntaxKind::Minus) => {
            let m = p.start();

            let op = PrefixOp::Neg;
            let ((), right_binding_power) = op.binding_power();

            // Eat the operator’s token.
            p.bump();

            expr_binding_power(p, right_binding_power);

            m.complete(p, SyntaxKind::PrefixExpr)
        }
        Some(SyntaxKind::LParen) => {
            let m = p.start();

            p.bump();
            expr_binding_power(p, 0);

            assert_eq!(p.peek(), Some(SyntaxKind::RParen));
            p.bump();

            m.complete(p, SyntaxKind::ParenExpr)
        }
        _ => return, // we’ll handle errors later.
    };

    loop {
        let op = match p.peek() {
            Some(SyntaxKind::Plus) => InfixOp::Add,
            Some(SyntaxKind::Minus) => InfixOp::Sub,
            Some(SyntaxKind::Star) => InfixOp::Mul,
            Some(SyntaxKind::Slash) => InfixOp::Div,
            _ => return, // we’ll handle errors later.
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            return;
        }

        // Eat the operator’s token.
        p.bump();

        let m = lhs.precede(p);
        expr_binding_power(p, right_binding_power);
        lhs = m.complete(p, SyntaxKind::BinaryExpr);
    }
}

#[cfg(test)]
mod tests {
    use super::super::check;
    use expect_test::expect;

    #[test]
    fn parse_number() {
        check(
            "123",
            expect![[r#"
Root@0..3
  Literal@0..3
    Number@0..3 "123""#]],
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
    fn parse_simple_binary_expression() {
        check(
            "1 +2",
            expect![[r#"
Root@0..4
  BinaryExpr@0..4
    Literal@0..2
      Number@0..1 "1"
      Whitespace@1..2 " "
    Plus@2..3 "+"
    Literal@3..4
      Number@3..4 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1 +  2  +  3 +4",
            expect![[r#"
Root@0..15
  BinaryExpr@0..15
    BinaryExpr@0..13
      BinaryExpr@0..8
        Literal@0..2
          Number@0..1 "1"
          Whitespace@1..2 " "
        Plus@2..3 "+"
        Whitespace@3..5 "  "
        Literal@5..8
          Number@5..6 "2"
          Whitespace@6..8 "  "
      Plus@8..9 "+"
      Whitespace@9..11 "  "
      Literal@11..13
        Number@11..12 "3"
        Whitespace@12..13 " "
    Plus@13..14 "+"
    Literal@14..15
      Number@14..15 "4""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1 + 2 * 3 - 4",
            expect![[r#"
Root@0..13
  BinaryExpr@0..13
    BinaryExpr@0..10
      Literal@0..2
        Number@0..1 "1"
        Whitespace@1..2 " "
      Plus@2..3 "+"
      Whitespace@3..4 " "
      BinaryExpr@4..10
        Literal@4..6
          Number@4..5 "2"
          Whitespace@5..6 " "
        Star@6..7 "*"
        Whitespace@7..8 " "
        Literal@8..10
          Number@8..9 "3"
          Whitespace@9..10 " "
    Minus@10..11 "-"
    Whitespace@11..12 " "
    Literal@12..13
      Number@12..13 "4""#]],
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
    Literal@2..4
      Number@2..4 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20 + 20",
            expect![[r#"
Root@0..8
  BinaryExpr@0..8
    PrefixExpr@0..4
      Minus@0..1 "-"
      Literal@1..4
        Number@1..3 "20"
        Whitespace@3..4 " "
    Plus@4..5 "+"
    Whitespace@5..6 " "
    Literal@6..8
      Number@6..8 "20""#]],
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
              Literal@7..9
                Number@7..9 "10"
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
  BinaryExpr@0..13
    Literal@0..2
      Number@0..1 "5"
      Whitespace@1..2 " "
    Star@2..3 "*"
    Whitespace@3..5 "  "
    ParenExpr@5..13
      LParen@5..6 "("
      Whitespace@6..7 " "
      BinaryExpr@7..12
        Literal@7..9
          Number@7..8 "2"
          Whitespace@8..9 " "
        Plus@9..10 "+"
        Whitespace@10..11 " "
        Literal@11..12
          Number@11..12 "1"
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
  Literal@3..7
    Number@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_number_followed_by_whitespace() {
        check(
            "999   ",
            expect![[r#"
Root@0..6
  Literal@0..6
    Number@0..3 "999"
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
  Literal@1..9
    Number@1..4 "123"
    Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 # Add one
  + 10 # Add ten",
            expect![[r##"
Root@0..35
  Whitespace@0..1 "\n"
  BinaryExpr@1..35
    BinaryExpr@1..21
      Literal@1..5
        Number@1..2 "1"
        Whitespace@2..5 "\n  "
      Plus@5..6 "+"
      Whitespace@6..7 " "
      Literal@7..21
        Number@7..8 "1"
        Whitespace@8..9 " "
        Comment@9..18 "# Add one"
        Whitespace@18..21 "\n  "
    Plus@21..22 "+"
    Whitespace@22..23 " "
    Literal@23..35
      Number@23..25 "10"
      Whitespace@25..26 " "
      Comment@26..35 "# Add ten""##]],
        );
    }
}
