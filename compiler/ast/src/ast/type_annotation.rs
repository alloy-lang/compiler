#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TypeAnnotation, fields: [name, type_, named_type_variables]);

impl TypeAnnotation {
    #[must_use]
    pub fn name(&self) -> Option<IdentOrOp> {
        first_ident_or_op_ident(self)
    }

    #[must_use]
    pub fn type_(&self) -> Option<Type> {
        self.0.children_with_tokens().find_map(Type::cast)
    }

    #[must_use]
    pub fn named_type_variables(&self) -> Vec<NamedTypeVariable> {
        children(self)
    }
}
