use crate::grammar::expr::parse_expr;

use super::*;

pub(crate) fn parse_lambda_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));

    let m = p.start();
    parse_arg_list(p);

    p.expect(TokenKind::RightArrow);

    let lambda_m = p.start();
    parse_expr(p);
    lambda_m.complete(p, SyntaxKind::LambdaExprBody);

    m.complete(p, SyntaxKind::LambdaExprDef)
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

        p.expect_with_recovery(TokenKind::Comma, &[TokenKind::Ident]);
    }

    p.expect_with_recovery(TokenKind::Pipe, &[TokenKind::RightArrow]);

    return m.complete(p, SyntaxKind::LambdaArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.at_set(&[TokenKind::Pipe, TokenKind::RightArrow]) || p.at_end()
    }
}

fn parse_arg(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.expect(TokenKind::Ident);

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
    fn parse_single_arg_lambda_expr() {
        check(
            "|arg1| -> 2",
            expect![[r#"
                Root@0..11
                  LambdaExprDef@0..11
                    LambdaArgList@0..7
                      Pipe@0..1 "|"
                      LambdaArg@1..5
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
    fn parse_multi_arg_lambda_expr() {
        check(
            "|arg1, arg2, arg3| -> 17",
            expect![[r#"
Root@0..24
  LambdaExprDef@0..24
    LambdaArgList@0..19
      Pipe@0..1 "|"
      LambdaArg@1..5
        Ident@1..5 "arg1"
      Comma@5..6 ","
      Whitespace@6..7 " "
      LambdaArg@7..11
        Ident@7..11 "arg2"
      Comma@11..12 ","
      Whitespace@12..13 " "
      LambdaArg@13..17
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
            "|arg1 arg2| -> 8",
            expect![[r#"
Root@0..16
  LambdaExprDef@0..16
    LambdaArgList@0..12
      Pipe@0..1 "|"
      LambdaArg@1..6
        Ident@1..5 "arg1"
        Whitespace@5..6 " "
      LambdaArg@6..10
        Ident@6..10 "arg2"
      Pipe@10..11 "|"
      Whitespace@11..12 " "
    RightArrow@12..14 "->"
    Whitespace@14..15 " "
    LambdaExprBody@15..16
      IntLiteral@15..16
        Integer@15..16 "8"
error at 6..10: expected ‘,’, but found identifier"#]],
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
        Ident@1..5 "arg1"
      Comma@5..6 ","
      Whitespace@6..7 " "
      LambdaArg@7..12
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
