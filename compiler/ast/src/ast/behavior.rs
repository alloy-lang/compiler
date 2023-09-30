#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(BehaviorDef, fields: [trait_, type_, members]);

impl BehaviorDef {
    #[must_use]
    pub fn trait_(&self) -> Option<Type> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::BehaviorTraitName {
                token.children_with_tokens().find_map(Type::cast)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::BehaviorTypeName {
                token.children_with_tokens().find_map(Type::cast)
            } else {
                None
            }
        })
    }

    pub fn members(&self) -> Vec<BehaviorMember> {
        self.0.children().filter_map(BehaviorMember::cast).collect()
    }
}

//
// Behavior Members
//

#[derive(Debug)]
pub enum BehaviorMember {
    TypeVariable(TypeVariable),
    TypeAnnotation(TypeAnnotation),
    Value(ValueDefinition),
}

impl BehaviorMember {
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
