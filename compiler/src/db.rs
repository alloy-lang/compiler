#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct Compiler {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Compiler {}
impl alloy_workspace::WorkspaceDatabase for Compiler {}

pub trait CompilerDatabase: alloy_workspace::WorkspaceDatabase {}
