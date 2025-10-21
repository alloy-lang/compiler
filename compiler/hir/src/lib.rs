use alloy_ast as ast;
use alloy_ast::AstElement;
use non_empty_vec::NonEmpty;
use std::fmt;
use std::sync::Arc;

mod ast_glossary;
mod hir;

pub use hir::*;

mod index;
#[cfg(test)]
mod tests;

pub use hir::lower_source_file;

#[salsa::jar(db = HirDatabase)]
pub struct Jar(
    LoweringErrors,
    LoweringWarnings,
    AstSourceFile,
    lower,
    HirModule,
    Import,
    lower_imports,
);

pub trait HirDatabase: salsa::DbWithJar<Jar> {
    fn upcast_hir(&self) -> &dyn HirDatabase;
}

impl<DB> HirDatabase for DB
where
    DB: Sized + salsa::DbWithJar<Jar>,
{
    fn upcast_hir(&self) -> &dyn HirDatabase {
        self
    }
}

#[salsa::input]
pub struct AstSourceFile {
    #[return_ref]
    pub source_file: Arc<ast::SourceFile>,
}

#[salsa::accumulator]
pub struct LoweringErrors(LoweringError);

#[salsa::accumulator]
pub struct LoweringWarnings(LoweringWarning);

#[salsa::tracked]
pub fn lower(db: &dyn HirDatabase, ast: AstSourceFile) -> HirModule {
    let mut errors = vec![];
    // let mut warnings = vec![];

    let imports = lower_imports(db, ast);
    errors.append(&mut lower_imports::accumulated::<LoweringErrors>(db, ast));
    // let type_definitions = lower_type_definitions(db, ast);
    // let traits = lower_traits(db, ast);
    // let behaviors = lower_behaviors(db, ast);
    // let values = lower_values(db, ast);

    lower_source_file(db, ast.source_file(db))
}

#[salsa::tracked]
pub fn lower_imports(db: &dyn HirDatabase, ast: AstSourceFile) -> Vec<Import> {
    let source_file = ast.source_file(db);

    source_file
        .statements()
        .iter()
        .filter_map(|stmt| match stmt {
            ast::Statement::ImportDef(i) => Some(i),
            _ => None,
        })
        .flat_map(|ast_import| {
            let children = ast_import
                .children()
                .into_iter()
                .enumerate()
                .collect::<Vec<_>>();
            let Some(((_, first), rest)) = children.split_first() else {
                unreachable!("parsing error")
            };

            let mut imports = vec![];
            match gather_all_import_segments(first, rest) {
                Ok(all_import_segments) => {
                    for import_segments in all_import_segments {
                        let (last, path) = import_segments.split_last();
                        imports.push(Import::new(db, path.to_vec(), last.clone()));
                    }
                }
                Err(error) => {
                    LoweringErrors::push(db, LoweringError::new(error, ast_import.range()));
                }
            };

            imports
        })
        .collect()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fqn {
    // todo: consider replacing module path with the module id
    module: NonEmpty<Name>,
    name: Name,
    sub_path: Vec<Name>,
}

impl Fqn {
    #[inline]
    pub fn new(
        module: impl IntoIterator<Item = impl Into<Name>>,
        name: impl Into<Name>,
        path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Self {
        unsafe {
            Self {
                module: NonEmpty::new_unchecked(module.into_iter().map(Into::into).collect()),
                name: name.into(),
                sub_path: path.into_iter().map(Into::into).collect(),
            }
        }
    }
}
