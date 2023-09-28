#[allow(clippy::wildcard_imports)]
use super::*;

pub struct Trait(SyntaxNode);

impl Trait {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TraitDef {
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
    pub fn members(&self) -> impl Iterator<Item = TraitMember> {
        self.0.children().filter_map(TraitMember::cast)
    }
}

impl fmt::Debug for Trait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trait")
            .field("name", &self.name())
            .field("members", &self.members().collect::<Vec<_>>())
            .finish()
    }
}

//
// Trait Members
//

#[derive(Debug)]
pub enum TraitMember {
    TypeVariable(TypeVariable),
    TypeAnnotation(TypeAnnotation),
    Value(ValueDefinition),
}

impl TraitMember {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::TypeVariable => Self::TypeVariable(TypeVariable::cast(node)?),
            SyntaxKind::TypeAnnotation => Self::TypeAnnotation(TypeAnnotation::cast(node)?),
            SyntaxKind::VariableDef => Self::Value(ValueDefinition::cast(node)?),
            _ => return None,
        };

        Some(result)
    }
}
