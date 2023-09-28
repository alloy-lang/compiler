#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub enum TypeVariable {
    Named(NamedTypeVariable),
    Self_(SelfTypeVariable),
}

impl TypeVariable {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::TypeVariable => {
                if let Some(named) = NamedTypeVariable::cast(node.clone()) {
                    Self::Named(named)
                } else if let Some(self_) = SelfTypeVariable::cast(node.clone()) {
                    Self::Self_(self_)
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        Some(result)
    }
}

pub struct NamedTypeVariable(SyntaxNode);

impl NamedTypeVariable {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeVariable {
            node.children_with_tokens()
                .filter_map(SyntaxElement::into_token)
                .find(|token| token.kind() == SyntaxKind::TypevarKw)?;
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
            .map(|token| token.text().into())
    }

    #[must_use]
    pub fn constraints(&self) -> impl Iterator<Item = TypeVariableConstraint> {
        self.0.children().filter_map(TypeVariableConstraint::cast)
    }
}

impl fmt::Debug for NamedTypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NamedTypeVariable")
            .field("name", &self.name())
            .field("constraints", &self.constraints().collect::<Vec<_>>())
            .finish()
    }
}

pub struct SelfTypeVariable(SyntaxNode);

impl SelfTypeVariable {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeVariable {
            node.children_with_tokens()
                .filter_map(SyntaxElement::into_token)
                .find(|token| token.kind() == SyntaxKind::SelfKw)?;
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn constraints(&self) -> impl Iterator<Item = TypeVariableConstraint> {
        self.0.children().filter_map(TypeVariableConstraint::cast)
    }
}

impl fmt::Debug for SelfTypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SelfTypeVariable")
            .field("constraints", &self.constraints().collect::<Vec<_>>())
            .finish()
    }
}

#[derive(Debug)]
pub enum TypeVariableConstraint {
    Trait(TypeVariableTraitConstraint),
    Kind(TypeVariableKindConstraint),
}

impl TypeVariableConstraint {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::TypeVariableKindConstraint => {
                Self::Kind(TypeVariableKindConstraint::cast(node)?)
            }
            SyntaxKind::TypeVariableTraitConstraint => {
                Self::Trait(TypeVariableTraitConstraint::cast(node)?)
            }
            _ => return None,
        };

        Some(result)
    }
}

pub struct TypeVariableKindConstraint(SyntaxNode);

impl TypeVariableKindConstraint {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeVariableKindConstraint {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn arity(&self) -> usize {
        self.0
            .children_with_tokens()
            .filter(|node| node.kind() == SyntaxKind::NilIdentifier)
            .count()
    }
}

impl fmt::Debug for TypeVariableKindConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeVariableKindConstraint")
            .field("arity", &self.arity())
            .finish()
    }
}

pub struct TypeVariableTraitConstraint(SyntaxNode);

impl TypeVariableTraitConstraint {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TypeVariableTraitConstraint {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn trait_(&self) -> Option<Path> {
        Path::cast(self.0.clone())
    }
}

impl fmt::Debug for TypeVariableTraitConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeVariableTraitConstraint")
            .field("trait_", &self.trait_())
            .finish()
    }
}
