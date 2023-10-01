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
