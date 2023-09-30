#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TypeAnnotation, fields: [name, type_]);

impl TypeAnnotation {
    #[must_use]
    pub fn name(&self) -> Option<String> {
        first_ident(self)
    }

    #[must_use]
    pub fn type_(&self) -> Option<Type> {
        self.0.children_with_tokens().find_map(Type::cast)
    }
}
