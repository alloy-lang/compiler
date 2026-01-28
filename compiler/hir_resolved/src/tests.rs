use alloy_hir as hir;
use alloy_workspace::{ModuleId, SourceFile, Workspace, WorkspaceDatabase};
use std::fs;

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestHirResDatabase {
    storage: salsa::Storage<Self>,
    workspace: Workspace,
}

#[salsa::db]
impl salsa::Database for TestHirResDatabase {}

#[salsa::db]
impl alloy_workspace::WorkspaceDatabase for TestHirResDatabase {
    fn add_module(&'_ mut self, slug: &str, path: &camino::Utf8Path, contents: &str) -> ModuleId {
        let prepared = alloy_workspace::prepare_module(self, slug, path, contents);
        self.workspace.insert_prepared_module(prepared)
    }

    fn get_source(&'_ self, module_id: ModuleId) -> SourceFile<'_> {
        self.workspace.get_source(module_id)
    }

    fn find_module_by_slug(&self, slug: &str) -> Option<ModuleId> {
        self.workspace.find_module_by_slug(self, slug)
    }
}

#[salsa::db]
impl hir::HirDatabase for TestHirResDatabase {}

impl TestHirResDatabase {
    pub(crate) fn new_with_stdlib() -> Self {
        let mut db = TestHirResDatabase::default();
        db.add_module(
            "std::option",
            camino::Utf8Path::new("/std/src/option.alloy"),
            fs::read_to_string("../../std/src/option.alloy")
                .expect("Expected to read std/src/option.alloy")
                .as_str(),
        );

        db
    }
}
