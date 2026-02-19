use alloy_hir as hir;
use alloy_workspace::WorkspaceDatabase;
use std::fs;

alloy_test_harness::test_database!(TestHirResDatabase: hir::HirDatabase);

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
