#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(SourceFile, fields: [module, statements]);

impl SourceFile {
    pub fn module(&self) -> Option<ModuleDef> {
        first_child(self)
    }

    pub fn statements(&self) -> Vec<Statement> {
        children(self)
    }
}

ast_union_node!(Statement, kinds: [ModuleDef, TraitDef, BehaviorDef, TypeDefinition, TypeAnnotation, ValueDef], Expression);

impl Statement {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::ModuleDef => Self::ModuleDef(ModuleDef::cast(node)?),
            SyntaxKind::TraitDef => Self::TraitDef(TraitDef::cast(node)?),
            SyntaxKind::BehaviorDef => Self::BehaviorDef(BehaviorDef::cast(node)?),
            SyntaxKind::TypeDefinition => Self::TypeDefinition(TypeDefinition::cast(node)?),
            SyntaxKind::TypeAnnotation => Self::TypeAnnotation(TypeAnnotation::cast(node)?),
            SyntaxKind::ValueDef => Self::ValueDef(ValueDef::cast(node)?),
            _ => Self::Expression(Expression::cast(node)?),
        };

        Some(result)
    }
}
