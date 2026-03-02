use camino::Utf8Path;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::db]
pub trait WorkspaceDatabase: salsa::Database {
    fn add_module(&mut self, slug: &str, path: &Utf8Path, contents: &str) -> ModuleId;

    fn get_source(&'_ self, module_id: ModuleId) -> &'_ RawSourceFile;

    fn get_virtual_source(&'_ self, module_id: VirtualModuleId) -> &'_ VirtualSourceFile;

    fn find_module_by_slug(&self, slug: &str) -> Option<ModuleId>;

    fn find_virtual_module_by_slug(&self, slug: &str) -> Option<VirtualModuleId>;
}

/// Represents a module identifier using :: syntax (e.g., "std::collections::HashMap")
#[salsa::interned(no_lifetime, debug)]
pub struct ModuleId {
    pub path: String,
}

impl std::fmt::Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        salsa::plumbing::with_attached_database(|db| write!(f, "{}", self.path(db)))
            .unwrap_or_else(|| write!(f, "{:?}", self))
    }
}

#[salsa::interned(no_lifetime, debug)]
pub struct VirtualModuleId {
    pub path: String,
}

impl std::fmt::Display for VirtualModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        salsa::plumbing::with_attached_database(|db| write!(f, "{}", self.path(db)))
            .unwrap_or_else(|| write!(f, "{:?}", self))
    }
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
    virtual_files: HashMap<VirtualModuleId, VirtualSourceFile>,
}

/// Prepared module data ready to be inserted into a workspace
///
/// This struct holds all Salsa-tracked items that have been created,
/// allowing the two-phase approach to avoid borrow checker conflicts.
pub struct PreparedModule {
    module_id: ModuleId,
    file: RawSourceFile,
    virtual_entries: Vec<(VirtualModuleId, String)>,
}

pub fn prepare_module(
    db: &dyn WorkspaceDatabase,
    slug: &str,
    path: &Utf8Path,
    contents: &str,
) -> PreparedModule {
    let module_id = ModuleId::new(db, slug.to_string());
    let file = RawSourceFile::new(db, Arc::from(path.as_str()), Arc::from(contents));

    // Prepare virtual module data
    let virtual_slugs = Workspace::compound_slugs(slug);
    let virtual_entries: Vec<(VirtualModuleId, String)> = virtual_slugs
        .iter()
        .map(|&virtual_slug| {
            let virtual_module_id = VirtualModuleId::new(db, virtual_slug.to_string());
            (virtual_module_id, virtual_slug.to_string())
        })
        .collect();

    PreparedModule {
        module_id,
        file,
        virtual_entries,
    }
}

impl Workspace {
    pub fn empty() -> Workspace {
        Workspace {
            raw_files: HashMap::default(),
            virtual_files: HashMap::default(),
        }
    }

    pub fn insert_prepared_module(&mut self, prepared: PreparedModule) -> ModuleId {
        let module_id = prepared.module_id;

        // Insert the raw source file
        self.raw_files.insert(prepared.module_id, prepared.file);

        // Insert virtual file entries
        for (virtual_module_id, name) in prepared.virtual_entries {
            self.virtual_files
                .entry(virtual_module_id)
                .or_insert_with(|| VirtualSourceFile {
                    name,
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

    pub fn maybe_get_source(&'_ self, module_id: ModuleId) -> Option<&'_ RawSourceFile> {
        self.raw_files.get(&module_id)
    }

    pub fn get_source(&'_ self, module_id: ModuleId) -> &'_ RawSourceFile {
        self.maybe_get_source(module_id).unwrap_or_else(|| {
            panic!(
                "Module '{module_id:?}' not found in workspace. Available modules: {:?}",
                self.raw_files.keys().collect::<Vec<_>>()
            )
        })
    }

    pub fn get_virtual_source(&'_ self, module_id: VirtualModuleId) -> &'_ VirtualSourceFile {
        self.virtual_files.get(&module_id).unwrap_or_else(|| {
            panic!(
                "Virtual module '{module_id:?}' not found in workspace. Available virtual modules: {:?}",
                self.virtual_files.keys().collect::<Vec<_>>()
            )
        })
    }

    pub fn find_module_by_slug(&self, db: &dyn WorkspaceDatabase, slug: &str) -> Option<ModuleId> {
        let module_id = ModuleId::new(db, slug.to_string());
        if self.raw_files.contains_key(&module_id) {
            Some(module_id)
        } else {
            None
        }
    }

    pub fn find_virtual_module_by_slug(
        &self,
        db: &dyn WorkspaceDatabase,
        slug: &str,
    ) -> Option<VirtualModuleId> {
        let module_id = VirtualModuleId::new(db, slug.to_string());
        if self.virtual_files.contains_key(&module_id) {
            Some(module_id)
        } else {
            None
        }
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
            Workspace::compound_slugs("really::long::module::path"),
            vec!["really", "really::long", "really::long::module",]
        );

        // short module path
        assert_eq!(Workspace::compound_slugs("short"), Vec::<&str>::new(),);
    }
}
