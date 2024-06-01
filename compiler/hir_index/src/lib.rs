use alloy_ast as ast;
use std::fmt;
use text_size::TextRange;
use alloy_ast::AstElement;
use crate::import::IndexedImport;

#[cfg(test)]
mod tests;
mod import;

//
// Name
//

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpannedName(Name, TextRange);

impl From<ast::Ident> for SpannedName {
    fn from(ident: ast::Ident) -> Self {
        Self(Name::new(ident.text()), ident.range())
    }
}

impl From<ast::IdentOrOp> for SpannedName {
    fn from(ident: ast::IdentOrOp) -> Self {
        Self(Name::new(ident.text()), ident.range())
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<&str> for Name {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl Name {
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        let name = name.trim_start_matches('(').trim_end_matches(')');

        Self(name.into())
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        Self(name)
    }
}
impl From<&str> for Name {
    fn from(name: &str) -> Self {
        Self(name.to_string())
    }
}
impl From<&String> for Name {
    fn from(name: &String) -> Self {
        Self(name.to_string())
    }
}

#[derive(Debug)]
pub struct IndexingCtx {
    imports: Vec<IndexedImport>,
    warnings: Vec<IndexingWarning>,
    errors: Vec<IndexingError>,
}

impl IndexingCtx {
    pub fn new() -> Self {
        Self {
            imports: Vec::new(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn finish(self) -> IndexedModule {
        IndexedModule {
            imports: self.imports,
            warnings: self.warnings,
            errors: self.errors,
        }
    }

    pub fn add_import(&mut self, import: IndexedImport) {
        self.imports.push(import);
    }

    fn warning(&mut self, kind: IndexingWarningKind, range: TextRange) {
        self.warnings.push(IndexingWarning { kind, range });
    }

    fn error(&mut self, kind: IndexingErrorKind, range: TextRange) {
        self.errors.push(IndexingError { kind, range });
    }
}

#[derive(Debug)]
pub struct IndexingError {
    kind: IndexingErrorKind,
    range: TextRange,
}

#[derive(Debug)]
pub enum IndexingErrorKind {
    ConflictingValue {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ImportGroupNotAtEnd {
        group_range: TextRange,
        position: usize,
        num_segments: usize,
    },
}

#[derive(Debug)]
pub struct IndexingWarning {
    kind: IndexingWarningKind,
    range: TextRange,
}

#[derive(Debug)]
pub enum IndexingWarningKind {
    DuplicateImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
}

#[derive(Debug)]
pub struct IndexedModule {
    imports: Vec<IndexedImport>,
    warnings: Vec<IndexingWarning>,
    errors: Vec<IndexingError>,
}

impl IndexedModule {}

#[must_use]
pub fn index_source_file(source_file: &ast::SourceFile) -> IndexedModule {
    let mut ctx = IndexingCtx::new();
    for statement in source_file.statements() {
        match statement {
            ast::Statement::ImportDef(import) => {
                import::index(&mut ctx, &import);
            }
            ast::Statement::Expression(_) => {}
            ast::Statement::ModuleDef(_) => {}
            ast::Statement::TraitDef(_) => {}
            ast::Statement::BehaviorDef(_) => {}
            ast::Statement::TypeDefinition(_) => {}
            ast::Statement::TypeAnnotation(_) => {}
            ast::Statement::ValueDef(_) => {}
        }
    }

    ctx.finish()
}

#[must_use]
pub fn index_repl_line(source_file: &ast::SourceFile) -> IndexedModule {
    index_source_file(source_file)
}
