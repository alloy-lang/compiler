#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub struct Value {
    pub value: ExpressionIdx,
    pub ast: ast::ValueDef,
}

pub(super) fn lower_value(ctx: &mut LoweringCtx, value: &ast::ValueDef) {
    //    todo!()
}
