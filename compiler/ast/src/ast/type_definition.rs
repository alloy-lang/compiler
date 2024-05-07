#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(TypeDefinition, fields: [name, type_args, types]);

ast_token!(Ident, fields: [text]);
ast_token!(OpIdent, fields: [text]);
ast_union_node!(IdentOrOp, kinds: [Ident, OpIdent]);

impl TypeDefinition {
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        first_ident(self)
    }

    #[must_use]
    pub fn type_args(&self) -> Vec<Ident> {
        all_matching_children(self, SyntaxKind::BoundedTypeArg)
            .into_iter()
            .collect()
    }

    #[must_use]
    pub fn types(&self) -> Vec<TypeDefinitionMember> {
        children(self)
    }
}

ast_node!(TypeDefinitionMember, fields: [name, properties]);

impl TypeDefinitionMember {
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        first_ident(self)
    }

    #[must_use]
    pub fn properties(&self) -> Vec<Type> {
        all_matching_children(self, SyntaxKind::TypeDefinitionMemberProperty)
    }
}
