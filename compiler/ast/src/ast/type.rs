#[allow(clippy::wildcard_imports)]
use super::*;

ast_union_node!(Type, kinds: [
    SelfType,
    NilIdentifier,
    TypeIdentifier,
    LambdaType,
    TupleType,
    BoundedType,
    ParenthesizedType
]);

ast_node!(SelfType, fields: []);
ast_token!(NilIdentifier, fields: []);
ast_node!(TypeIdentifier, fields: [name]);

impl TypeIdentifier {
    #[must_use]
    pub fn name(&self) -> Option<Path> {
        first_child(self)
    }
}

ast_node!(LambdaType, fields: [arg_type, return_type]);

impl LambdaType {
    #[must_use]
    pub fn arg_type(&self) -> Option<Type> {
        self.0.children_with_tokens().filter_map(Type::cast).nth(0)
    }

    #[must_use]
    pub fn return_type(&self) -> Option<Type> {
        self.0.children_with_tokens().filter_map(Type::cast).nth(1)
    }
}

ast_node!(TupleType, fields: [members]);

impl TupleType {
    pub fn members(&self) -> Vec<Type> {
        self.0
            .children_with_tokens()
            .filter_map(Type::cast)
            .collect()
    }
}

ast_node!(BoundedType, fields: [base, args]);

impl BoundedType {
    pub fn base(&self) -> Option<Type> {
        match_node(self, SyntaxKind::BoundedTypeBase)?
            .children_with_tokens()
            .find_map(Type::cast)
    }

    pub fn args(&self) -> Vec<Type> {
        match_nodes(self, SyntaxKind::BoundedTypeArg)
            .flat_map(|node| node.children_with_tokens().filter_map(Type::cast))
            .collect()
    }
}

ast_node!(ParenthesizedType, fields: [inner]);

impl ParenthesizedType {
    #[must_use]
    pub fn inner(&self) -> Option<Type> {
        self.0.children_with_tokens().find_map(Type::cast)
    }
}
