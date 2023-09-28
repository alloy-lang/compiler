#[allow(clippy::wildcard_imports)]
use super::*;

pub struct SourceFile(SyntaxNode);

impl SourceFile {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::SourceFile {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn module(&self) -> Option<Module> {
        self.0.children().find_map(Module::cast)
    }

    pub fn statements(&self) -> Vec<Statement> {
        self.0.children().filter_map(Statement::cast).collect()
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("module", &self.module())
            .finish()
    }
}

#[derive(Debug)]
pub enum Statement {
    Module(Module),
    Trait(Trait),
    Behavior(Behavior),
    TypeDefinition(TypeDefinition),
    TypeAnnotation(TypeAnnotation),
    Value(ValueDefinition),
    Expression(Expression),
}

impl Statement {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::ModuleDef => Self::Module(Module::cast(node)?),
            SyntaxKind::TraitDef => Self::Trait(Trait::cast(node)?),
            SyntaxKind::BehaviorDef => Self::Behavior(Behavior::cast(node)?),
            SyntaxKind::TypeDef => Self::TypeDefinition(TypeDefinition::cast(node)?),
            SyntaxKind::TypeAnnotation => Self::TypeAnnotation(TypeAnnotation::cast(node)?),
            SyntaxKind::VariableDef => Self::Value(ValueDefinition::cast(node)?),
            _ => Self::Expression(Expression::cast(node)?),
        };

        Some(result)
    }
}
