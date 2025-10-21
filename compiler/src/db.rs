#[derive(Default)]
#[salsa::db(alloy_workspace::Jar, alloy_hir::Jar)]
pub(crate) struct Compiler {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Compiler {}

pub trait CompilerDatabase: alloy_workspace::WorkspaceDatabase + alloy_hir::HirDatabase {}
