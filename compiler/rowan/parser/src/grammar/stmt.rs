use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(variable_def(p))
    } else {
        expr::parse_expr(p)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Equals);

    expr::parse_expr(p);

    m.complete(p, SyntaxKind::VariableDef)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
Root@0..13
  VariableDef@0..13
    LetKw@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..7 "foo"
    Whitespace@7..8 " "
    Equals@8..9 "="
    Whitespace@9..10 " "
    VariableRef@10..13
      Ident@10..13 "bar""#]],
        );
    }

    #[test]
    fn parse_variable_definition_lambda() {
        check(
            "let add = |a, b| -> a + b",
            expect![[r#"
Root@0..25
  VariableDef@0..25
    LetKw@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..7 "add"
    Whitespace@7..8 " "
    Equals@8..9 "="
    Whitespace@9..10 " "
    LambdaExprDef@10..25
      LambdaArgList@10..17
        Pipe@10..11 "|"
        LambdaArg@11..12
          VariableRef@11..12
            Ident@11..12 "a"
        Comma@12..13 ","
        Whitespace@13..14 " "
        LambdaArg@14..15
          VariableRef@14..15
            Ident@14..15 "b"
        Pipe@15..16 "|"
        Whitespace@16..17 " "
      RightArrow@17..19 "->"
      Whitespace@19..20 " "
      LambdaExprBody@20..25
        InfixExpr@20..25
          VariableRef@20..22
            Ident@20..21 "a"
            Whitespace@21..22 " "
          Plus@22..23 "+"
          Whitespace@23..24 " "
          VariableRef@24..25
            Ident@24..25 "b""#]],
        );
    }

    #[test]
    fn recover_variable_definition_lambda() {
        check(
            r#"
            let add1 = |a, 9| -
            let add2 = |a, b| -> a + b
            "#,
            expect![[r#"
                Root@0..84
                  Whitespace@0..13 "\n            "
                  VariableDef@13..45
                    LetKw@13..16 "let"
                    Whitespace@16..17 " "
                    Ident@17..21 "add1"
                    Whitespace@21..22 " "
                    Equals@22..23 "="
                    Whitespace@23..24 " "
                    LambdaExprDef@24..45
                      LambdaArgList@24..31
                        Pipe@24..25 "|"
                        LambdaArg@25..26
                          VariableRef@25..26
                            Ident@25..26 "a"
                        Comma@26..27 ","
                        Whitespace@27..28 " "
                        LambdaArg@28..29
                          IntLiteral@28..29
                            Integer@28..29 "9"
                        Pipe@29..30 "|"
                        Whitespace@30..31 " "
                      Error@31..45
                        Minus@31..32 "-"
                        Whitespace@32..45 "\n            "
                      LambdaExprBody@45..45
                  VariableDef@45..84
                    LetKw@45..48 "let"
                    Whitespace@48..49 " "
                    Ident@49..53 "add2"
                    Whitespace@53..54 " "
                    Equals@54..55 "="
                    Whitespace@55..56 " "
                    LambdaExprDef@56..84
                      LambdaArgList@56..63
                        Pipe@56..57 "|"
                        LambdaArg@57..58
                          VariableRef@57..58
                            Ident@57..58 "a"
                        Comma@58..59 ","
                        Whitespace@59..60 " "
                        LambdaArg@60..61
                          VariableRef@60..61
                            Ident@60..61 "b"
                        Pipe@61..62 "|"
                        Whitespace@62..63 " "
                      RightArrow@63..65 "->"
                      Whitespace@65..66 " "
                      LambdaExprBody@66..84
                        InfixExpr@66..84
                          VariableRef@66..68
                            Ident@66..67 "a"
                            Whitespace@67..68 " "
                          Plus@68..69 "+"
                          Whitespace@69..70 " "
                          VariableRef@70..84
                            Ident@70..71 "b"
                            Whitespace@71..84 "\n            "
                error at 31..32: expected ‘->’, but found ‘-’
                error at 45..48: expected integer, fractional, string, char, identifier, ‘-’, ‘(’, ‘if‘ or ‘|’, but found ‘let’"#]],
        );
    }

    #[test]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a",
            expect![[r#"
                Root@0..17
                  VariableDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Equals@6..7 "="
                    Whitespace@7..8 "\n"
                  VariableDef@8..17
                    LetKw@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..13 "b"
                    Whitespace@13..14 " "
                    Equals@14..15 "="
                    Whitespace@15..16 " "
                    VariableRef@16..17
                      Ident@16..17 "a"
                error at 8..11: expected integer, fractional, string, char, identifier, ‘-’, ‘(’, ‘if‘ or ‘|’, but found ‘let’"#]],
        );
    }

    #[test]
    fn recover_on_let_token_2() {
        check(
            "let 1\nlet b = a",
            expect![[r#"
                Root@0..15
                  VariableDef@0..6
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Error@4..6
                      Integer@4..5 "1"
                      Whitespace@5..6 "\n"
                  VariableDef@6..15
                    LetKw@6..9 "let"
                    Whitespace@9..10 " "
                    Ident@10..11 "b"
                    Whitespace@11..12 " "
                    Equals@12..13 "="
                    Whitespace@13..14 " "
                    VariableRef@14..15
                      Ident@14..15 "a"
                error at 4..5: expected identifier, but found integer
                error at 6..9: expected ‘=’, but found ‘let’
                error at 6..9: expected integer, fractional, string, char, identifier, ‘-’, ‘(’, ‘if‘ or ‘|’, but found ‘let’"#]],
        );
    }
}
