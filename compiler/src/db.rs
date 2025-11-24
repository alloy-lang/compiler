use alloy_workspace::{ModuleId, SourceFile, Workspace};

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct CompilerDatabase {
    storage: salsa::Storage<Self>,
    workspace: Workspace,
}

#[salsa::db]
impl salsa::Database for CompilerDatabase {}

#[salsa::db]
impl alloy_workspace::WorkspaceDatabase for CompilerDatabase {
    fn add_module(&mut self, slug: &str, path: &camino::Utf8Path, contents: &str) -> ModuleId {
        self.workspace.add_module(self, slug, path, contents)
    }

    fn get_source(&'_ self, module_id: ModuleId) -> SourceFile<'_> {
        self.workspace.get_source(module_id)
    }
}

#[salsa::db]
impl alloy_hir::HirDatabase for CompilerDatabase {}

#[salsa::db]
impl alloy_hir_typed::HirTyDatabase for CompilerDatabase {}
