use alloy_ast as ast;
use alloy_ast::AstElement;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub(crate) struct AstGlossary {
    type_definitions: FxHashMap<String, ast::TypeDefinition>,
    value_definitions: FxHashMap<String, ast::ValueDef>,
}

impl AstGlossary {
    pub(crate) fn get_value_by_name(&self, name: &String) -> Option<&ast::ValueDef> {
        self.value_definitions.get(name)
    }
    pub(crate) fn get_type_definition_by_name(
        &self,
        name: &String,
    ) -> Option<&ast::TypeDefinition> {
        self.type_definitions.get(name)
    }
}

//
// Builders
//

impl AstGlossary {
    fn new() -> Self {
        Self {
            type_definitions: FxHashMap::default(),
            value_definitions: FxHashMap::default(),
        }
    }

    pub(crate) fn summarize_repl_line(ast: &ast::SourceFile) -> Self {
        let mut glossary = Self::new();

        for statement in ast.statements() {
            match statement {
                ast::Statement::TypeDefinition(type_definition) => {
                    glossary.add_type_definition(type_definition);
                }
                ast::Statement::ValueDef(value_definition) => {
                    glossary.add_value_definition(value_definition);
                }
                _ => {}
            }
        }

        glossary
    }

    pub(crate) fn summarize_source_file(ast: &ast::SourceFile) -> Self {
        let mut glossary = Self::new();

        if let Some(module) = ast.module() {
            for type_definition in module.type_definitions() {
                glossary.add_type_definition(type_definition);
            }
            for value_definition in module.values() {
                glossary.add_value_definition(value_definition);
            }
        }

        glossary
    }

    fn add_type_definition(&mut self, ast: ast::TypeDefinition) {
        let Some(name) = ast.name() else {
            return;
        };

        self.type_definitions.insert(name.text(), ast);
    }

    fn add_value_definition(&mut self, ast: ast::ValueDef) {
        let Some(name) = ast.name() else {
            return;
        };

        self.value_definitions.insert(name.text(), ast);
    }
}
