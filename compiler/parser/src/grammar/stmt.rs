#[allow(clippy::wildcard_imports)]
use super::*;
use crate::grammar::expr::EXPR_FIRSTS;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(variable_def(p))
    } else if p.at(TokenKind::ImportKw) {
        Some(import::parse_import(p))
    } else if p.at(TokenKind::TraitKw) {
        Some(r#trait::parse_trait(p))
    } else if p.at(TokenKind::BehaviorKw) {
        Some(behavior::parse_behavior(p))
    } else if p.at(TokenKind::TypedefKw) {
        Some(type_definition::parse_type_definition(p))
    } else if p.at(TokenKind::TypeOfKw) {
        Some(type_annotation::parse_type_annotation(
            p,
            r#type::ParseMode::OutsideSelfContext,
            ts![],
        ))
    } else {
        expr::parse_expr(p, ParseErrorContext::TopLevelExpr)
    }
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    ident::parse_ident_or_op(
        p,
        ParseErrorContext::VariableDefIdent,
        ts![TokenKind::Equals],
    );
    p.expect_with_recovery(
        TokenKind::Equals,
        ParseErrorContext::VariableDefEquals,
        EXPR_FIRSTS,
    );

    expr::parse_expr(p, ParseErrorContext::VariableDefExpr);

    m.complete(p, SyntaxKind::VariableDef)
}
