use std::collections::HashMap;
use std::sync::Arc;

#[salsa::db]
pub trait WorkspaceDatabase: salsa::Database {}

#[salsa::interned]
pub struct FileSlug<'db> {
    #[returns(ref)]
    pub raw_path: Arc<str>,
}

#[salsa::input]
pub struct RawSourceFile {
    #[returns(ref)]
    pub raw_path: Arc<str>,
    #[returns(ref)]
    pub contents: Arc<str>,
}

#[salsa::input]
pub struct Workspace {
    #[returns(ref)]
    pub files: HashMap<String, RawSourceFile>,
}

impl Workspace {
    pub fn empty(db: &dyn WorkspaceDatabase) -> Workspace {
        Workspace::new(db, HashMap::default())
    }
}

#[salsa::interned]
pub struct PackageId<'db> {
    #[returns(ref)]
    pub raw: Arc<str>,
}

#[salsa::tracked]
pub struct Package<'db> {
    pub id: PackageId<'db>,
    #[returns(ref)]
    pub files: Vec<RawSourceFile>,
}

impl<'db> Package<'db> {
    pub fn contains(self, db: &'db dyn WorkspaceDatabase, path: &str) -> bool {
        self.files(db)
            .iter()
            .any(|f| f.raw_path(db).as_ref() == path)
    }
}

/// Parses a source file and returns the parse errors.
/// This query is cached by salsa, so repeated calls with the same file
/// will return the cached result unless the file contents have changed.
#[salsa::tracked]
pub fn parse_errors(
    db: &dyn WorkspaceDatabase,
    file: RawSourceFile,
) -> Vec<alloy_parser::ParseError> {
    let (_, errors) = alloy_ast::source_file(file.contents(db));
    errors
}
