use alloy_workspace::{ModuleId, RawSourceFile, VirtualModuleId, VirtualSourceFile, Workspace};
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

        if let Some(existing_raw) = self.workspace.maybe_get_source(module_id) {
            let existing_raw = *existing_raw;
            existing_raw.set_raw_path(self).to(Arc::from(path.as_str()));
            existing_raw.set_contents(self).to(Arc::from(contents));
            return module_id;
        }

        let prepared = alloy_workspace::prepare_module(self, slug, path, contents);
        self.workspace.insert_prepared_module(prepared)
    }

    fn get_source(&'_ self, module_id: ModuleId) -> &'_ RawSourceFile {
        self.workspace.get_source(module_id)
    }

    fn get_virtual_source(&'_ self, module_id: VirtualModuleId) -> &'_ VirtualSourceFile {
        self.workspace.get_virtual_source(module_id)
    }

    fn find_module_by_slug(&self, slug: &str) -> Option<ModuleId> {
        self.workspace.find_module_by_slug(self, slug)
    }

    fn find_virtual_module_by_slug(&self, slug: &str) -> Option<VirtualModuleId> {
        self.workspace.find_virtual_module_by_slug(self, slug)
    }
}

#[salsa::db]
impl alloy_ast::AstDatabase for CompilerDatabase {}

#[salsa::db]
impl alloy_hir_def::HirDefDatabase for CompilerDatabase {}

#[salsa::db]
impl alloy_hir_typed::HirTyDatabase for CompilerDatabase {}
