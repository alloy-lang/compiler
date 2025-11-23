use std::collections::HashMap;
use std::sync::Arc;

#[salsa::db]
pub trait WorkspaceDatabase: salsa::Database {}

#[salsa::interned]
pub struct FileSlug<'db> {
    #[returns(ref)]
    pub raw_path: Arc<str>,
}

/// Represents a module identifier using :: syntax (e.g., "std::collections::HashMap")
#[salsa::interned]
pub struct ModuleId<'db> {
    /// The module path as a string (e.g., "std::collections")
    #[returns(ref)]
    pub path: Arc<str>,
}

impl<'db> ModuleId<'db> {
    /// Create a ModuleId from a slice of path segments
    pub fn from_segments(db: &'db dyn WorkspaceDatabase, segments: &[impl AsRef<str>]) -> Self {
        let path = segments
            .iter()
            .map(|s| s.as_ref())
            .collect::<Vec<_>>()
            .join("::");
        ModuleId::new(db, Arc::from(path))
    }

    /// Get the segments of this module path
    pub fn segments(&self, db: &'db dyn WorkspaceDatabase) -> Vec<String> {
        self.path(db)
            .split("::")
            .map(|s| s.to_string())
            .collect()
    }

    /// Get the file system path for this module (e.g., "std::collections" -> "std/collections.alloy")
    pub fn file_path(&self, db: &'db dyn WorkspaceDatabase) -> String {
        let segments = self.segments(db);
        let mut path = segments.join("/");
        path.push_str(".alloy");
        path
    }
}

#[salsa::input]
pub struct RawSourceFile {
    #[returns(ref)]
    pub raw_path: Arc<str>,
    #[returns(ref)]
    pub contents: Arc<str>,
}

/// Workspace containing all source files indexed by ModuleId
/// This is not a Salsa struct to avoid hashing overhead of the HashMap
#[derive(Clone, PartialEq, Eq)]
pub struct Workspace<'db> {
    files: HashMap<ModuleId<'db>, RawSourceFile>,
}

impl<'db> std::hash::Hash for Workspace<'db> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash the number of entries
        self.files.len().hash(state);

        // Collect entries and hash them
        // Note: HashMap iteration order is not deterministic across runs,
        // but it's consistent within a run which is sufficient for Salsa caching
        for (module_id, file) in &self.files {
            module_id.hash(state);
            file.hash(state);
        }
    }
}

impl<'db> Workspace<'db> {
    pub fn new(files: HashMap<ModuleId<'db>, RawSourceFile>) -> Self {
        Workspace { files }
    }

    pub fn empty() -> Workspace<'db> {
        Workspace {
            files: HashMap::default(),
        }
    }

    /// Get a file by its ModuleId
    pub fn get_file(&self, module_id: ModuleId<'db>) -> Option<RawSourceFile> {
        self.files.get(&module_id).copied()
    }

    /// Get all module IDs in the workspace
    pub fn module_ids(&self) -> Vec<ModuleId<'db>> {
        self.files.keys().copied().collect()
    }

    /// Get all files in the workspace
    pub fn files(&self) -> &HashMap<ModuleId<'db>, RawSourceFile> {
        &self.files
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
