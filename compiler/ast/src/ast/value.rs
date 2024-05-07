#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ValueDef, fields: [name, value]);

impl ValueDef {
    #[must_use]
    pub fn name(&self) -> Option<IdentOrOp> {
        first_ident_or_op_ident(self)
    }

    #[must_use]
    pub fn value(&self) -> Option<Expression> {
        first_child(self)
    }
}
