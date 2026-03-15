use alloy_hir_def as hir;

alloy_test_harness::test_database!(TestHirResDatabase: hir::HirDefDatabase);

impl TestHirResDatabase {
    pub(crate) fn new_with_stdlib() -> Self {
        TestHirResDatabase::default()
    }
}
