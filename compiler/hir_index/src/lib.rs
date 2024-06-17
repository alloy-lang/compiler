use crate::import::IndexedImport;
use crate::value::IndexedValue;
use alloy_ast as ast;
use alloy_ast::AstElement;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry::Occupied;
use std::fmt;
use text_size::TextRange;

mod import;
#[cfg(test)]
mod tests;
mod value;

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

#[derive(Debug, Default)]
pub struct IndexingCtx {
    imports: FxHashMap<Name, IndexedImport>,
    values: FxHashMap<Name, IndexedValue>,
    warnings: Vec<IndexingWarning>,
    errors: Vec<IndexingError>,
}

impl IndexingCtx {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    fn finish(self) -> IndexedModule {
        IndexedModule {
            imports: self.imports,
            values: self.values,
            warnings: self.warnings,
            errors: self.errors,
        }
    }

    fn add_import(&mut self, import: IndexedImport) {
        if let Occupied(existing) = self.imports.entry(import.last.0.clone()) {
            let existing = existing.get();
            let first = existing.range();
            let second = import.range();

            if import.equivalent(existing) {
                self.warning(
                    IndexingWarningKind::DuplicateImport {
                        name: import.last.0,
                        first,
                        second,
                    },
                    second,
                );
            } else {
                self.error(
                    IndexingErrorKind::ConflictingImport {
                        name: import.last.0,
                        first,
                        second,
                    },
                    second,
                );
            }
        } else {
            self.imports.insert(import.last.0.clone(), import);
        }
    }

    fn add_value(&mut self, value: IndexedValue) {
        if let Occupied(existing) = self.values.entry(value.name.0.clone()) {
            let first = existing.get().name.1;
            let second = value.name.1;
            self.error(
                IndexingErrorKind::ConflictingValue {
                    name: value.name.0,
                    first,
                    second,
                },
                second,
            );
        } else {
            self.values.insert(value.name.0.clone(), value);
        }
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
    imports: FxHashMap<Name, IndexedImport>,
    values: FxHashMap<Name, IndexedValue>,
    warnings: Vec<IndexingWarning>,
    errors: Vec<IndexingError>,
}

impl IndexedModule {
    #[must_use]
    pub fn warnings(&self) -> &[IndexingWarning] {
        &self.warnings
    }

    #[must_use]
    pub fn errors(&self) -> &[IndexingError] {
        &self.errors
    }
}

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
            ast::Statement::ValueDef(value) => {
                value::index(&mut ctx, &value);
            }
        }
    }

    ctx.finish()
}

#[must_use]
pub fn index_repl_line(source_file: &ast::SourceFile) -> IndexedModule {
    index_source_file(source_file)
}
