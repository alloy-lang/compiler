use alloy_rowan_lexer::TokenKind;
use alloy_rowan_syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

mod expr;
mod stmt;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        stmt::stmt(p);
    }

    m.complete(p, SyntaxKind::Root)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1\na",
            expect![[r#"
Root@0..11
  VariableDef@0..10
    LetKw@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..5 "a"
    Whitespace@5..6 " "
    Equals@6..7 "="
    Whitespace@7..8 " "
    IntLiteral@8..10
      Integer@8..9 "1"
      Whitespace@9..10 "\n"
  VariableRef@10..11
    Ident@10..11 "a""#]],
        );
    }
}
