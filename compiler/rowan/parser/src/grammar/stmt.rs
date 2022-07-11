use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(SyntaxKind::LetKw) {
        Some(variable_def(p))
    } else {
        expr::expr(p)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(SyntaxKind::LetKw));
    let m = p.start();
    p.bump();

    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Equals);

    expr::expr(p);

    m.complete(p, SyntaxKind::VariableDef)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

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
                error at 8..11: expected number, identifier, ‘-’ or ‘(’, but found ‘let’"#]],
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
                      Number@4..5 "1"
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
                error at 4..5: expected identifier, but found number
                error at 6..9: expected ‘=’, but found ‘let’
                error at 6..9: expected number, identifier, ‘-’ or ‘(’, but found ‘let’"#]],
        );
    }
}
