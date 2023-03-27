use super::*;
use crate::grammar::expr::EXPR_FIRSTS;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(variable_def(p))
    } else if p.at(TokenKind::ImportKw) {
        Some(import::parse_import(p))
    } else if p.at(TokenKind::TraitKw) {
        Some(r#trait::parse_trait(p))
    } else if p.at(TokenKind::TypeOfKw) {
        Some(r#type::parse_type_annotation(p, TokenSet::EMPTY))
    } else {
        expr::parse_expr(p, ParseErrorContext::TopLevelExpr)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::VariableDefIdent,
        TokenSet::new([TokenKind::Equals]),
    );
    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::VariableDefEquals,
        EXPR_FIRSTS,
    );

    expr::parse_expr(p, ParseErrorContext::VariableDefExpr);

    m.complete(p, SyntaxKind::VariableDef)
}
