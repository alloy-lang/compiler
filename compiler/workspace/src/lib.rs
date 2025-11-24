use camino::Utf8Path;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::db]
pub trait WorkspaceDatabase: salsa::Database {
    fn add_module(&mut self, slug: &str, path: &Utf8Path, contents: &str) -> ModuleId;

    fn get_source(&'_ self, module_id: ModuleId) -> SourceFile<'_>;

    fn find_module_by_slug(&self, slug: &str) -> Option<ModuleId> {
        let module_id = ModuleId::new(self, Arc::from(slug));
        match self.get_source(module_id) {
            SourceFile::Raw(_) | SourceFile::Virtual(_) => Some(module_id),
        }
    }
}

/// Represents a module identifier using :: syntax (e.g., "std::collections::HashMap")
#[salsa::interned(no_lifetime, debug)]
pub struct ModuleId {
    /// The module path as a string (e.g., "std::collections")
    #[returns(ref)]
    pub path: Arc<str>,
}

impl<'db> ModuleId {
    /// Get the segments of this module path
    pub fn segments(&self, db: &'db dyn WorkspaceDatabase) -> Vec<String> {
        self.path(db).split("::").map(|s| s.to_string()).collect()
    }

    /// Get the file system path for this module (e.g., "std::collections" -> "std/collections.alloy")
    pub fn file_path(&self, db: &'db dyn WorkspaceDatabase) -> String {
        let segments = self.segments(db);
        let mut path = segments.join("/");
        path.push_str(".alloy");
        path
    }
}

pub enum SourceFile<'a> {
    Raw(&'a RawSourceFile),
    Virtual(&'a VirtualSourceFile),
}

#[salsa::input]
pub struct RawSourceFile {
    #[returns(ref)]
    pub raw_path: Arc<str>,
    #[returns(ref)]
    pub contents: Arc<str>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct VirtualSourceFile {
    pub name: String,
    pub children: Vec<ModuleId>,
}

/// Workspace containing all source files indexed by ModuleId
/// This is not a Salsa struct to avoid hashing overhead of the HashMap
#[derive(Default, Clone, PartialEq, Eq)]
pub struct Workspace {
    raw_files: HashMap<ModuleId, RawSourceFile>,
    virtual_files: HashMap<ModuleId, VirtualSourceFile>,
}

impl Workspace {
    pub fn empty() -> Workspace {
        Workspace {
            raw_files: HashMap::default(),
            virtual_files: HashMap::default(),
        }
    }

    pub fn add_module<'db>(
        &mut self,
        db: &'db dyn WorkspaceDatabase,
        slug: &str,
        path: &Utf8Path,
        contents: &str,
    ) -> ModuleId {
        let module_id = ModuleId::new(db, Arc::from(slug));
        let file = RawSourceFile::new(db, Arc::from(path.as_str()), Arc::from(contents));
        self.raw_files.insert(module_id, file);

        let virtual_slugs = Workspace::compound_slugs(slug);

        for virtual_slug in virtual_slugs {
            let virtual_module_id = ModuleId::new(db, Arc::from(virtual_slug));
            self.virtual_files
                .entry(virtual_module_id)
                .or_insert_with(|| VirtualSourceFile {
                    name: virtual_slug.to_string(),
                    children: Vec::new(),
                })
                .children
                .push(module_id);
        }

        module_id
    }

    fn compound_slugs(slug: &str) -> Vec<&str> {
        let mut result = Vec::new();

        let bytes = slug.as_bytes();

        for i in 0..slug.len().saturating_sub(1) {
            if bytes[i] == b':' && bytes[i + 1] == b':' {
                result.push(&slug[0..i]);
            }
        }

        result
    }

    pub fn get_source(&self, module_id: ModuleId) -> SourceFile {
        self.raw_files
            .get(&module_id)
            .map(SourceFile::Raw)
            .or_else(|| self.virtual_files.get(&module_id).map(SourceFile::Virtual))
            .expect("module ID not found")
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

#[cfg(test)]
mod tests {
    use crate::Workspace;

    #[test]
    fn compound_slug() {
        // long module path
        assert_eq!(
            Workspace::<'_>::compound_slugs("really::long::module::path"),
            vec!["really", "really::long", "really::long::module",]
        );

        // short module path
        assert_eq!(Workspace::<'_>::compound_slugs("short"), Vec::<&str>::new(),);
    }
}
