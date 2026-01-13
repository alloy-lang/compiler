use alloy_workspace::{ModuleId, SourceFile, Workspace};

#[salsa::db]
#[derive(Default, Clone)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
    workspace: Workspace,
}

#[salsa::db]
impl salsa::Database for CompilerDatabase {}

#[salsa::db]
impl alloy_workspace::WorkspaceDatabase for CompilerDatabase {
    fn add_module(&mut self, slug: &str, path: &camino::Utf8Path, contents: &str) -> ModuleId {
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
impl alloy_hir::HirDatabase for CompilerDatabase {}

#[salsa::db]
impl alloy_hir_typed::HirTyDatabase for CompilerDatabase {}
