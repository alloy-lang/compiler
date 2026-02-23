use alloy_project::{ModuleFile, Project};
use expect_test::expect_file;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::path::{Path, PathBuf};
use std::{env, fs};

/// Generates a salsa test database struct with `WorkspaceDatabase` implemented.
///
/// The struct gets `#[salsa::db]`, `#[derive(Default, Clone)]`, and standard
/// `salsa::Database` + `WorkspaceDatabase` impls. Additional salsa database
/// traits can be listed after the struct name.
///
/// # Example
///
/// ```ignore
/// alloy_test_harness::test_database!(pub(crate) TestDb: hir::HirDatabase, crate::MyDatabase);
/// ```
#[macro_export]
macro_rules! test_database {
    ($name:ident $(: $($trait:path),+ $(,)?)?) => {
        #[salsa::db]
        #[derive(Default, Clone)]
        pub(crate) struct $name {
            storage: salsa::Storage<Self>,
            workspace: alloy_workspace::Workspace,
        }

        #[salsa::db]
        impl salsa::Database for $name {}

        #[salsa::db]
        impl alloy_workspace::WorkspaceDatabase for $name {
            fn add_module(
                &mut self,
                slug: &str,
                path: &camino::Utf8Path,
                contents: &str,
            ) -> alloy_workspace::ModuleId {
                let prepared = alloy_workspace::prepare_module(self, slug, path, contents);
                self.workspace.insert_prepared_module(prepared)
            }

            fn get_source(
                &'_ self,
                module_id: alloy_workspace::ModuleId,
            ) -> alloy_workspace::SourceFile<'_> {
                self.workspace.get_source(module_id)
            }

            fn find_module_by_slug(&self, slug: &str) -> Option<alloy_workspace::ModuleId> {
                self.workspace.find_module_by_slug(self, slug)
            }
        }

        $($(
            #[salsa::db]
            impl $trait for $name {}
        )+)?
    };
}

#[macro_export]
macro_rules! idx {
    ($idx:expr) => {
        la_arena::Idx::from_raw(la_arena::RawIdx::from_u32($idx))
    };
}

#[macro_export]
macro_rules! expr_idx {
    ($db:expr, $module_id:expr, $name:expr) => {{
        let (hir_module, _) = alloy_hir::lower_file($db, $module_id);
        hir_module
            .get_expression_by_name(&alloy_hir::Name::new($name), alloy_scope::Scopes::ROOT)
            .expect(&format!("failed to find '{}' in {:#?}", $name, hir_module))
            .0
    }};
}

/// # Panics
///
/// Will panic if tests fail.
#[track_caller]
pub fn run_test_case(
    test_path: PathBuf,
    test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe + UnwindSafe,
) {
    let test_content = fs::read_to_string(&test_path).unwrap();
    let (input, _expected) = test_content.split_once("\n===\n").unwrap();

    let result = test_fn(&test_path, input);

    let expected_test_content = format!("{input}\n===\n{result}\n");
    expect_file![test_path].assert_eq(&expected_test_content);
}

/// # Panics
///
/// Will panic if tests fail.
#[track_caller]
pub fn run_test_dir(
    tests_dir: &str,
    test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe + UnwindSafe,
) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{tests_dir}"))
    };

    let mut failed_tests = vec![];

    for entry in fs::read_dir(&tests_dir).unwrap() {
        let test_path = entry.unwrap().path().canonicalize().unwrap();

        println!(
            "\n==== RUNNING TEST [{:?}] {:?} ====",
            &tests_dir,
            test_path.file_stem().unwrap()
        );

        let file_name = test_path.file_name().unwrap().to_os_string();

        if test_path.ends_with("test") {
            continue;
        }

        let did_panic = std::panic::catch_unwind(|| {
            run_test_case(test_path, &test_fn);
        })
        .is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    failed_tests.sort();

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}

/// # Panics
///
/// Will panic if tests fail.
///
/// init takes all module files and returns C, which can be used to initialize state for the test function.
#[track_caller]
pub fn run_std_lib_tests<C: RefUnwindSafe>(
    init: impl Fn(&[&ModuleFile]) -> C + RefUnwindSafe + UnwindSafe,
    test_fn: impl Fn(&C, &ModuleFile) + RefUnwindSafe + UnwindSafe,
) {
    let project = Project::new("../../std").expect("expected project to be created");

    let mut failed_tests = vec![];
    let context = init(&project.modules().collect::<Vec<_>>());
    for module_file in project.modules() {
        let path = module_file.path();

        println!(
            "\n==== RUNNING STD LIB TEST {:?} ====",
            path.file_stem().unwrap()
        );

        let did_panic = std::panic::catch_unwind(|| {
            test_fn(&context, module_file);
        })
        .is_err();

        if did_panic {
            failed_tests.push(path);
        }
    }

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}
