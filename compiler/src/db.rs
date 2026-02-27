use alloy_workspace::{ModuleId, SourceFile, Workspace};
use salsa::Setter;
use std::sync::Arc;

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
        let module_id = ModuleId::new(self, slug.to_string());

        if let Some(SourceFile::Raw(existing_raw)) = self.workspace.maybe_get_source(module_id) {
            let existing_raw = existing_raw.clone();
            existing_raw.set_raw_path(self).to(Arc::from(path.as_str()));
            existing_raw.set_contents(self).to(Arc::from(contents));
            return module_id;
        }

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
