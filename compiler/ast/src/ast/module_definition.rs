#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub enum ModuleDefinition {
    Import(Import),
    Trait(Trait),
    Behavior(Behavior),
    TypeDefinition(TypeDefinition),
    TypeAnnotation(TypeAnnotation),
    Value(ValueDefinition),
}

impl ModuleDefinition {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::ImportStatement => Self::Import(Import::cast(node)?),
            SyntaxKind::TraitDef => Self::Trait(Trait::cast(node)?),
            SyntaxKind::BehaviorDef => Self::Behavior(Behavior::cast(node)?),
            SyntaxKind::TypeDef => Self::TypeDefinition(TypeDefinition::cast(node)?),
            SyntaxKind::TypeAnnotation => Self::TypeAnnotation(TypeAnnotation::cast(node)?),
            SyntaxKind::VariableDef => Self::Value(ValueDefinition::cast(node)?),
            _ => return None,
        };

        Some(result)
    }
}
