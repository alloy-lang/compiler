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
        match node.kind() {
            SyntaxKind::SelfType => Some(Self::SelfRef),
            SyntaxKind::NilIdentifier => Some(Self::NilRef),
            SyntaxKind::TypeIdentifier => {
                Some(Self::Identifier(TypeIdentifier::cast(node.into_node()?)?))
            }
            SyntaxKind::LambdaType => Some(Self::Lambda(LambdaType::cast(node.into_node()?)?)),
            SyntaxKind::TupleType => Some(Self::Tuple(TupleType::cast(node.into_node()?)?)),
            SyntaxKind::BoundedType => Some(Self::Bounded(BoundedType::cast(node.into_node()?)?)),
            SyntaxKind::ParenthesizedType => Some(Self::Parenthesized(ParenthesizedType::cast(
                node.into_node()?,
            )?)),
            _ => None,
        }
    }
}

pub struct TypeIdentifier(SyntaxNode);

impl TypeIdentifier {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeIdentifier {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn name(&self) -> Option<Path> {
        Path::cast(self.0.clone())
    }
}

impl fmt::Debug for TypeIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeIdentifier")
            .field("name", &self.name())
            .finish()
    }
}

pub struct LambdaType(SyntaxNode);

impl LambdaType {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::LambdaType {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn arg_type(&self) -> Option<Type> {
        self.0.children_with_tokens().filter_map(Type::cast).nth(0)
    }

    #[must_use]
    pub fn return_type(&self) -> Option<Type> {
        self.0.children_with_tokens().filter_map(Type::cast).nth(1)
    }
}

impl fmt::Debug for LambdaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LambdaType")
            .field("arg_type", &self.arg_type())
            .field("return_type", &self.return_type())
            .finish()
    }
}

pub struct TupleType(SyntaxNode);

impl TupleType {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TupleType {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn members(&self) -> impl Iterator<Item = Type> {
        self.0.children_with_tokens().filter_map(Type::cast)
    }
}

impl fmt::Debug for TupleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TupleType")
            .field("members", &self.members().collect::<Vec<_>>())
            .finish()
    }
}

pub struct BoundedType(SyntaxNode);

impl BoundedType {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::BoundedType {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn base(&self) -> Option<Type> {
        self.0
            .children()
            .find(|node| node.kind() == SyntaxKind::BoundedTypeBase)
            .and_then(|node| node.children_with_tokens().next())
            .and_then(Type::cast)
    }

    pub fn args(&self) -> impl Iterator<Item = Type> {
        self.0
            .children()
            .filter(|node| node.kind() == SyntaxKind::BoundedTypeArg)
            .flat_map(|node| node.children_with_tokens())
            .filter_map(Type::cast)
    }
}

impl fmt::Debug for BoundedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BoundedType")
            .field("base", &self.base())
            .field("args", &self.args().collect::<Vec<_>>())
            .finish()
    }
}

pub struct ParenthesizedType(SyntaxNode);

impl ParenthesizedType {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ParenthesizedType {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn inner(&self) -> Option<Type> {
        self.0.children_with_tokens().find_map(Type::cast)
    }
}

impl fmt::Debug for ParenthesizedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParenthesizedType")
            .field("inner", &self.inner())
            .finish()
    }
}
