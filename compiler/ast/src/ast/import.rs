#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ImportDef, fields: [children]);

impl ImportDef {
    #[must_use]
    pub fn children(&self) -> Vec<ImportDefChild> {
        children(self)
    }
}

ast_union_node!(ImportDefChild, kinds: [ImportDefSegment, ImportDefGroup]);

ast_node!(ImportDefSegment, fields: [name]);

impl ImportDefSegment {
    #[must_use]
    pub fn name(&self) -> Option<IdentOrOp> {
        first_ident_or_op_ident(self)
    }
}

ast_node!(ImportDefGroup, fields: [children]);

impl ImportDefGroup {
    #[must_use]
    pub fn children(&self) -> Vec<ImportDefSegment> {
        children(self)
    }
}
