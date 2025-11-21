#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct Compiler {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for Compiler {}

#[salsa::db]
impl alloy_workspace::WorkspaceDatabase for Compiler {}

#[salsa::db]
impl alloy_hir::HirDatabase for Compiler {}

pub trait CompilerDatabase: alloy_hir::HirDatabase {}
