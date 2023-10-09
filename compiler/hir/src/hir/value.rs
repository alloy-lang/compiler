#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub struct Value {
    name: Name,
    value: ExpressionIdx,
}

pub(super) fn lower_value(ctx: &mut LoweringCtx, ast: &ast::ValueDef) {
    let Some(name) = ast.name() else {
        todo!("validation");
        return;
    };
    let name = Name::new(name);

    let Some(value) = ast.value() else {
        todo!("validation");
        return;
    };
    let value = lower_expression(ctx, &value);

    ctx.value(Value {
        name,
        value,
    });
}
