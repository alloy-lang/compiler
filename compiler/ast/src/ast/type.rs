#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub enum Type {
    SelfRef,
    NilRef,
    Identifier(TypeIdentifier),
    Lambda(LambdaType),
    Tuple(TupleType),
    Bounded(BoundedType),
    Parenthesized(ParenthesizedType),
}

impl Type {
    #[must_use]
    pub(crate) fn cast(node: SyntaxElement) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::SelfType => Self::SelfRef,
            SyntaxKind::NilIdentifier => Self::NilRef,
            SyntaxKind::TypeIdentifier => {
                Self::Identifier(TypeIdentifier::cast(node.into_node()?)?)
            }
            SyntaxKind::LambdaType => Self::Lambda(LambdaType::cast(node.into_node()?)?),
            SyntaxKind::TupleType => Self::Tuple(TupleType::cast(node.into_node()?)?),
            SyntaxKind::BoundedType => Self::Bounded(BoundedType::cast(node.into_node()?)?),
            SyntaxKind::ParenthesizedType => {
                Self::Parenthesized(ParenthesizedType::cast(node.into_node()?)?)
            }
            _ => return None,
        };

        Some(result)
    }
}

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
