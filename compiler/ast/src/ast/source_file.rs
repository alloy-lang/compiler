#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(SourceFile, fields: [module, statements]);

impl SourceFile {
    #[must_use]
    pub fn module(&self) -> Option<ModuleDef> {
        first_child(self)
    }

    #[must_use]
    pub fn statements(&self) -> Vec<Statement> {
        children(self)
    }
}

ast_union_node!(Statement, kinds: [ModuleDef, TraitDef, BehaviorDef, TypeDefinition, TypeAnnotation, ValueDef], Expression);
