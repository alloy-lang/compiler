#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_value(ctx: &mut LoweringCtx, ast: &ast::ValueDef) {
    let Some(name) = ast.name() else {
        // todo!("validation");
        return;
    };
    let name = Name::new(name);

    let value = match ast.value() {
        Some(value) => lower_expression_inner(ctx, &value),
        None => Expression::Missing,
    };

    ctx.add_value(name, value, &ast.syntax());
}
