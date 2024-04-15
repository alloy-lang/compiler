#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_value(ctx: &mut LoweringCtx, ast: &ast::ValueDef) {
    let value = match ast.value() {
        Some(value) => lower_expression_inner(ctx, &value),
        None => Expression::Missing,
    };

    let Some(name) = ast.name() else {
        // we can't add a value that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let name = Name::new(name);

    ctx.add_value(name, value, &ast.syntax());
}
