#[allow(clippy::wildcard_imports)]
use super::*;

ast_union_node!(Type, kinds: [
    SelfType,
    UnitType,
    NilIdentifier,
    TypeIdentifier,
    LambdaType,
    TupleType,
    ParenthesizedType,
    BoundedType
]);

ast_node!(SelfType, fields: []);
ast_node!(UnitType, fields: []);
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
    #[must_use]
    pub fn members(&self) -> Vec<Type> {
        children(self)
    }
}

ast_node!(ParenthesizedType, fields: [inner]);

impl ParenthesizedType {
    #[must_use]
    pub fn inner(&self) -> Option<Type> {
        first_child(self)
    }
}

ast_node!(BoundedType, fields: [base, args]);

impl BoundedType {
    #[must_use]
    pub fn base(&self) -> Option<Type> {
        first_matching_child(self, SyntaxKind::BoundedTypeBase)
    }

    #[must_use]
    pub fn args(&self) -> Vec<Type> {
        all_matching_children(self, SyntaxKind::BoundedTypeArg)
    }
}
