use std::collections::HashMap;
use std::sync::Arc;

#[salsa::jar(db = WorkspaceDatabase)]
pub struct Jar(FileSlug, RawSourceFile, Workspace, PackageId, Package);

pub trait WorkspaceDatabase: salsa::DbWithJar<Jar> {
    fn upcast_workspace(&self) -> &dyn WorkspaceDatabase;
}

impl<DB> WorkspaceDatabase for DB
where
    DB: Sized + salsa::DbWithJar<Jar>,
{
    fn upcast_workspace(&self) -> &dyn WorkspaceDatabase {
        self
    }
}

//
//
//

#[salsa::interned]
pub struct FileSlug {
    #[return_ref]
    raw_path: Arc<str>,
}

#[salsa::input]
pub struct RawSourceFile {
    pub slug: FileSlug,
    #[return_ref]
    pub contents: Arc<str>,
}

#[salsa::input]
pub struct Workspace {
    pub files: HashMap<FileSlug, RawSourceFile>,
}

impl Workspace {
    pub fn empty(db: &dyn WorkspaceDatabase) -> Workspace {
        Workspace::new(db, HashMap::default())
    }
}

#[salsa::interned]
pub struct PackageId {
    pub raw: Arc<str>,
}

#[salsa::tracked]
pub struct Package {
    pub id: PackageId,
    pub files: Vec<RawSourceFile>,
}

impl Package {
    pub fn contains(&self, db: &dyn WorkspaceDatabase, slug: FileSlug) -> bool {
        self.files(db).iter().any(|f| f.slug(db) == slug)
    }
}
