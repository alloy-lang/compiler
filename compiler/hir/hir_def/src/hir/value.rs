#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDefinition {
    pub name: Name,
    pub type_annotation: Option<TypeIdx>,
    pub expr_idx: ExpressionIdx,
}

#[salsa::tracked(debug)]
pub struct ValueDef<'db> {
    pub module_id: ModuleId,
    #[returns(ref)]
    pub name: Name,
    pub type_annotation: Option<TypeIdx>,
    pub expression_idx: ExpressionIdx,
}

pub(super) fn lower_value(
    ctx: &mut LoweringCtx,
    ast: &ast::ValueDef,
) -> Option<(Name, ExpressionIdx)> {
    let value = match ast.value() {
        Some(value) => lower_expression_inner(ctx, &value),
        None => Expression::Missing,
    };

    let Some(name) = ast.name() else {
        // we can't add a value that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return None;
    };
    let name = Name::new(name.text());

    Some((name.clone(), ctx.add_value(name, value, ast)))
}
