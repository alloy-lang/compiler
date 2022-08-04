use crate::grammar::expr::{
    parse_char_literal, parse_expr, parse_fractional_literal, parse_int_literal,
    parse_string_literal, parse_variable_ref,
};

use super::*;

const LAMBDA_ARG_SET: [TokenKind; 2] = [TokenKind::Ident, TokenKind::Integer];

pub(crate) fn parse_lambda_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));

    let lambda_m = p.start();
    parse_arg_list(p);

    p.expect(TokenKind::RightArrow);

    let body_m = p.start();
    parse_expr(p);
    body_m.complete(p, SyntaxKind::LambdaExprBody);

    lambda_m.complete(p, SyntaxKind::LambdaExprDef)
}

fn parse_arg_list(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));
    let m = p.start();
    p.bump();

    loop {
        if should_stop(p) {
            break;
        }

        parse_arg(p);

        if should_stop(p) {
            break;
        }

        p.expect_with_recovery(TokenKind::Comma, &LAMBDA_ARG_SET);
    }

    p.expect_with_recovery(TokenKind::Pipe, &[TokenKind::RightArrow]);

    return m.complete(p, SyntaxKind::LambdaArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(&[TokenKind::Pipe, TokenKind::RightArrow]) || p.at_end()
    }
}

fn parse_arg(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    if p.at(TokenKind::Integer) {
        parse_int_literal(p);
    } else if p.at(TokenKind::Fractional) {
        parse_fractional_literal(p);
    } else if p.at(TokenKind::String) {
        parse_string_literal(p);
    } else if p.at(TokenKind::Char) {
        parse_char_literal(p);
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p);
    } else {
        p.error();
    }

    m.complete(p, SyntaxKind::LambdaArg)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_no_arg_lambda_expr() {
        check(
            "|| -> 2",
            expect![[r#"
                Root@0..7
                  LambdaExprDef@0..7
                    LambdaArgList@0..3
                      Pipe@0..1 "|"
                      Pipe@1..2 "|"
                      Whitespace@2..3 " "
                    RightArrow@3..5 "->"
                    Whitespace@5..6 " "
                    LambdaExprBody@6..7
                      IntLiteral@6..7
                        Integer@6..7 "2""#]],
        );
    }

    #[test]
    fn parse_single_identifier_arg_lambda_expr() {
        check(
            "|arg1| -> 2",
            expect![[r#"
                Root@0..11
                  LambdaExprDef@0..11
                    LambdaArgList@0..7
                      Pipe@0..1 "|"
                      LambdaArg@1..5
                        VariableRef@1..5
                          Ident@1..5 "arg1"
                      Pipe@5..6 "|"
                      Whitespace@6..7 " "
                    RightArrow@7..9 "->"
                    Whitespace@9..10 " "
                    LambdaExprBody@10..11
                      IntLiteral@10..11
                        Integer@10..11 "2""#]],
        );
    }

    #[test]
    fn parse_single_int_literal_arg_lambda_expr() {
        check(
            "|0| -> 2",
            expect![[r#"
Root@0..8
  LambdaExprDef@0..8
    LambdaArgList@0..4
      Pipe@0..1 "|"
      LambdaArg@1..2
        IntLiteral@1..2
          Integer@1..2 "0"
      Pipe@2..3 "|"
      Whitespace@3..4 " "
    RightArrow@4..6 "->"
    Whitespace@6..7 " "
    LambdaExprBody@7..8
      IntLiteral@7..8
        Integer@7..8 "2""#]],
        );
    }

    #[test]
    fn parse_single_fractional_literal_arg_lambda_expr() {
        check(
            "|0.4| -> 2",
            expect![[r#"
                Root@0..10
                  LambdaExprDef@0..10
                    LambdaArgList@0..6
                      Pipe@0..1 "|"
                      LambdaArg@1..4
                        FractionalLiteral@1..4
                          Fractional@1..4 "0.4"
                      Pipe@4..5 "|"
                      Whitespace@5..6 " "
                    RightArrow@6..8 "->"
                    Whitespace@8..9 " "
                    LambdaExprBody@9..10
                      IntLiteral@9..10
                        Integer@9..10 "2""#]],
        );
    }

    #[test]
    fn parse_single_string_literal_arg_lambda_expr() {
        check(
            r#"|"hello"| -> 5"#,
            expect![[r#"
                Root@0..14
                  LambdaExprDef@0..14
                    LambdaArgList@0..10
                      Pipe@0..1 "|"
                      LambdaArg@1..8
                        StringLiteral@1..8
                          String@1..8 "\"hello\""
                      Pipe@8..9 "|"
                      Whitespace@9..10 " "
                    RightArrow@10..12 "->"
                    Whitespace@12..13 " "
                    LambdaExprBody@13..14
                      IntLiteral@13..14
                        Integer@13..14 "5""#]],
        );
    }

    #[test]
    fn parse_single_char_literal_arg_lambda_expr() {
        check(
            "|'c'| -> 1",
            expect![[r#"
                Root@0..10
                  LambdaExprDef@0..10
                    LambdaArgList@0..6
                      Pipe@0..1 "|"
                      LambdaArg@1..4
                        CharLiteral@1..4
                          Char@1..4 "'c'"
                      Pipe@4..5 "|"
                      Whitespace@5..6 " "
                    RightArrow@6..8 "->"
                    Whitespace@8..9 " "
                    LambdaExprBody@9..10
                      IntLiteral@9..10
                        Integer@9..10 "1""#]],
        );
    }

    #[test]
    fn parse_multi_arg_lambda_expr() {
        check(
            "|arg1, arg2, arg3| -> 17",
            expect![[r#"
Root@0..24
  LambdaExprDef@0..24
    LambdaArgList@0..19
      Pipe@0..1 "|"
      LambdaArg@1..5
        VariableRef@1..5
          Ident@1..5 "arg1"
      Comma@5..6 ","
      Whitespace@6..7 " "
      LambdaArg@7..11
        VariableRef@7..11
          Ident@7..11 "arg2"
      Comma@11..12 ","
      Whitespace@12..13 " "
      LambdaArg@13..17
        VariableRef@13..17
          Ident@13..17 "arg3"
      Pipe@17..18 "|"
      Whitespace@18..19 " "
    RightArrow@19..21 "->"
    Whitespace@21..22 " "
    LambdaExprBody@22..24
      IntLiteral@22..24
        Integer@22..24 "17""#]],
        );
    }

    #[test]
    fn parse_multi_arg_lambda_expr_no_comma() {
        check(
            "|arg1 1001 'c'| -> 8",
            expect![[r#"
                Root@0..20
                  LambdaExprDef@0..20
                    LambdaArgList@0..16
                      Pipe@0..1 "|"
                      LambdaArg@1..6
                        VariableRef@1..6
                          Ident@1..5 "arg1"
                          Whitespace@5..6 " "
                      LambdaArg@6..11
                        IntLiteral@6..11
                          Integer@6..10 "1001"
                          Whitespace@10..11 " "
                      Error@11..14
                        Char@11..14 "'c'"
                      Pipe@14..15 "|"
                      Whitespace@15..16 " "
                    RightArrow@16..18 "->"
                    Whitespace@18..19 " "
                    LambdaExprBody@19..20
                      IntLiteral@19..20
                        Integer@19..20 "8"
                error at 6..10: expected ‘,’, but found integer
                error at 11..14: expected ‘,’, but found char"#]],
        );
    }

    #[test]
    fn parse_lambda_expr_no_pipe() {
        check(
            "|arg1, arg2 -> 8",
            expect![[r#"
Root@0..16
  LambdaExprDef@0..16
    LambdaArgList@0..12
      Pipe@0..1 "|"
      LambdaArg@1..5
        VariableRef@1..5
          Ident@1..5 "arg1"
      Comma@5..6 ","
      Whitespace@6..7 " "
      LambdaArg@7..12
        VariableRef@7..12
          Ident@7..11 "arg2"
          Whitespace@11..12 " "
    RightArrow@12..14 "->"
    Whitespace@14..15 " "
    LambdaExprBody@15..16
      IntLiteral@15..16
        Integer@15..16 "8"
error at 12..14: expected ‘|’, but found ‘->’"#]],
        );
    }
}
