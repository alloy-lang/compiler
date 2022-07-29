use crate::grammar::expr::parse_expr;

use super::*;

pub(crate) fn parse_lambda_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Pipe));

    let m = p.start();
    parse_arg_list(p);

    p.expect(TokenKind::RightArrow);

    let lambda_m = p.start();
    parse_expr(p);
    lambda_m.complete(p, SyntaxKind::LambdaBodyExpr);

    m.complete(p, SyntaxKind::LambdaDefExpr)
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

        p.expect(TokenKind::Comma);
    }

    p.expect(TokenKind::Pipe);

    return m.complete(p, SyntaxKind::LambdaArgList);

    fn should_stop(p: &mut Parser) -> bool {
        p.at(TokenKind::Pipe)
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
                  LambdaDefExpr@0..7
                    LambdaArgList@0..3
                      Pipe@0..1 "|"
                      Pipe@1..2 "|"
                      Whitespace@2..3 " "
                    RightArrow@3..5 "->"
                    Whitespace@5..6 " "
                    LambdaBodyExpr@6..7
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
                  LambdaDefExpr@0..11
                    LambdaArgList@0..7
                      Pipe@0..1 "|"
                      LambdaArg@1..5
                        Ident@1..5 "arg1"
                      Pipe@5..6 "|"
                      Whitespace@6..7 " "
                    RightArrow@7..9 "->"
                    Whitespace@9..10 " "
                    LambdaBodyExpr@10..11
                      IntLiteral@10..11
                        Integer@10..11 "2""#]],
        );
    }
}
