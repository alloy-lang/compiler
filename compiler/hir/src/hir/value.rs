#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub struct Value {
    name: Name,
    value: ExpressionIdx,
}

pub(super) fn lower_value(ctx: &mut LoweringCtx, ast: &ast::ValueDef) {
    let value = match ast.value() {
        Some(value) => lower_expression(ctx, &value),
        None => ctx.add_missing_expression(&ast.syntax()),
    };

    let Some(name) = ast.name() else {
        // todo!("validation");
        return;
    };
    let name = Name::new(name);

    ctx.add_value(Value { name, value });
}
