#[allow(clippy::wildcard_imports)]
use super::*;
use alloy_ast::{AstElementPointer, Expression};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct IndexedValue {
    name: SpannedName,
    expression: AstElementPointer<Expression>,
}

pub(super) fn index(ctx: &mut IndexingCtx, value: &ast::ValueDef) {
    let Some(name) = value.name() else {
        return;
    };
    let Some(body) = value.value() else {
        return;
    };

    ctx.add_value(IndexedValue {
        name: name.into(),
        expression: AstElementPointer::new(body),
    });
}
