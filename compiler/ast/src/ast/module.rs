#[allow(clippy::wildcard_imports)]
use super::*;

pub struct Module(SyntaxNode);

impl Module {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ModuleDef {
            Some(Self(node))
        } else {
            None
        }
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Module")
            .field("definitions", &self.definitions().collect::<Vec<_>>())
            .finish()
    }
}

impl Module {
    pub fn definitions(&self) -> impl Iterator<Item = ModuleDefinition> {
        self.0.children().filter_map(ModuleDefinition::cast)
    }
}
