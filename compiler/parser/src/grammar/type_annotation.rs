#[allow(clippy::wildcard_imports)]
use super::*;

pub(crate) fn parse_type_annotation(
    p: &mut Parser,
    mode: r#type::ParseMode,
    parent_recovery_set: TokenSet,
) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::TypeOfKw);

    ident::parse_ident_or_op(p, ParseErrorContext::TypeOfName, ts![TokenKind::Colon]);
    p.expect_with_recovery(
        TokenKind::Colon,
        ParseErrorContext::TypeOfColon,
        r#type::SINGLE_TYPE_RECOVERY_SET,
    );

    r#type::parse_type(
        p,
        ParseErrorContext::TypeOfType,
        mode,
        ts![],
        parent_recovery_set,
    );

    if p.at(TokenKind::WhereKw) {
        parse_type_annotation_type_variables(p, parent_recovery_set);
    }

    m.complete(p, SyntaxKind::TypeAnnotation)
}

fn parse_type_annotation_type_variables(p: &mut Parser, parent_recovery_set: TokenSet) {
    fn should_stop(p: &mut Parser) -> bool {
        p.at_top_level_token_or_not_set(ts![TokenKind::TypevarKw])
    }

    p.bump(TokenKind::WhereKw);

    loop {
        if should_stop(p) {
            break;
        }

        parse_generic_type_variable(p, parent_recovery_set);
    }
}

fn parse_generic_type_variable(p: &mut Parser, parent_recovery_set: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.bump(TokenKind::TypevarKw);

    p.expect_with_recovery(
        TokenKind::Ident,
        ParseErrorContext::TypeVariableName,
        parent_recovery_set.plus(TokenKind::TypevarKw),
    );

    if p.maybe_at(TokenKind::Equals) {
        p.bump(TokenKind::Equals);

        type_variable::parse_typevar_constraints(p);
    }

    m.complete(p, SyntaxKind::NamedTypeVariable)
}
