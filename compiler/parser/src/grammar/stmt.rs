#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::LetKw) {
        Some(value::parse_value(p))
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
