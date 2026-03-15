use alloy_hir as hir;
use alloy_workspace::WorkspaceDatabase;

alloy_test_harness::test_database!(TestHirResDatabase: hir::HirDatabase);

impl TestHirResDatabase {
    pub(crate) fn new_with_stdlib() -> Self {
        TestHirResDatabase::default()
    }
}
