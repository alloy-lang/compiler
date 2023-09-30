#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ValueDef, fields: [name, value]);

impl ValueDef {
    #[must_use]
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    pub fn value(&self) -> Option<Expression> {
        first_child(self)
    }
}
