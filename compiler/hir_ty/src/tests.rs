use alloy_diagnostics::DiagnosticsReporter;
use alloy_hir as hir;
use alloy_workspace::{ModuleId, SourceFile, Workspace, WorkspaceDatabase};
use std::path::Path;
use std::{env, fs};

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestHirTyDatabase {
    storage: salsa::Storage<Self>,
    workspace: Workspace,
}

#[salsa::db]
impl salsa::Database for TestHirTyDatabase {}

#[salsa::db]
impl WorkspaceDatabase for TestHirTyDatabase {
    fn add_module(&mut self, slug: &str, path: &camino::Utf8Path, contents: &str) -> ModuleId {
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
impl hir::HirDatabase for TestHirTyDatabase {}

#[salsa::db]
impl crate::HirTyDatabase for TestHirTyDatabase {}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_ty_test(path, input, false, false, false)
    });
}

#[test]
fn repl_line_lowering_errors() {
    alloy_test_harness::run_test_dir("repl_line_lowering_errors", |path, input| {
        run_hir_ty_test(path, input, false, true, false)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_ty_test(path, input, true, false, false)
    });
}

#[test]
fn repl_line_type_checking_errors() {
    alloy_test_harness::run_test_dir("repl_line_type_checking_errors", |path, input| {
        run_hir_ty_test(path, input, false, false, true)
    });
}

#[test]
fn on_demand_test() {
    for arg in env::args() {
        if arg.contains("--test-case") {
            let test_case = arg.split("--test-case=").nth(1).unwrap();

            let tests_path = {
                let current_dir = env::current_dir().unwrap();
                current_dir.join(format!("src/tests/{test_case}"))
            };

            let did_panic = std::panic::catch_unwind(|| {
                alloy_test_harness::run_test_case(tests_path, |path, input| {
                    run_hir_ty_test(path, input, false, false, false)
                });
            })
            .is_err();

            assert!(!did_panic, "{} test failed", test_case,);

            break;
        }
    }
}

#[track_caller]
fn run_hir_ty_test(
    _path: &Path,
    input: &str,
    _expect_parse_errors: bool,
    _expect_lowering_errors: bool,
    _expect_type_checking_errors: bool,
) -> String {
    let mut db = TestHirTyDatabase::default();
    db.add_module(
        "test_data",
        camino::Utf8Path::new("./test/test_data.alloy"),
        r#"
    typedef Test[t] = Thing t
    let test = Test::Thing 0
    let new = |t| -> Test::Thing t
    "#,
    );
    db.add_module(
        "std::option",
        camino::Utf8Path::new("/std/src/option.alloy"),
        fs::read_to_string("../../std/src/option.alloy")
            .expect("Expected to read std/src/option.alloy")
            .as_str(),
    );
    db.add_module(
        "std::function",
        camino::Utf8Path::new("/std/src/function.alloy"),
        fs::read_to_string("../../std/src/function.alloy")
            .expect("Expected to read std/src/function.alloy")
            .as_str(),
    );
    let test_module_id = db.add_module("main", camino::Utf8Path::new("./test/main.alloy"), input);

    let (_hir_module, parse_errors) = hir::lower_file(&db, test_module_id);
    let typed_module = crate::type_check_module(&db, test_module_id);

    // let file_name = path.to_str().expect("Expected filename");
    // if expect_parse_errors {
    //     assert!(
    //         !parse_errors.is_empty(),
    //         "file '{}' did not contain parse errors",
    //         file_name
    //     );
    // } else {
    //     assert!(
    //         parse_errors.is_empty(),
    //         "file '{}' contained parse errors: {parse_errors:?}",
    //         file_name
    //     );
    // }
    // if expect_lowering_errors {
    //     assert!(
    //         !hir_module.errors().is_empty() || !hir_module.warnings().is_empty(),
    //         "file '{}' did not contain lowering errors or warnings",
    //         file_name
    //     );
    // } else {
    //     assert!(
    //         hir_module.errors().is_empty(),
    //         "file '{file_name}' contained lowering errors: {:?}",
    //         hir_module.errors(),
    //     );
    //     // TODO: decide if we want to fail tests on warnings
    //     // assert!(
    //     //     hir_module.warnings().is_empty(),
    //     //     "file '{file_name}' contained lowering warnings: {:?}",
    //     //     hir_module.warnings(),
    //     // );
    // }
    // if expect_type_checking_errors {
    //     assert!(
    //         !typed_module.errors().is_empty() || !typed_module.warnings().is_empty(),
    //         "file '{}' did not contain lowering errors or warnings",
    //         file_name
    //     );
    // } else {
    //     assert!(
    //         typed_module.errors().is_empty(),
    //         "file '{file_name}' contained type checking errors: {:?}",
    //         typed_module.errors(),
    //     );
    //     // TODO: decide if we want to fail tests on warnings
    //     // assert!(
    //     //     typed_module.warnings().is_empty(),
    //     //     "file '{file_name}' contained type checking warnings: {:?}",
    //     //     typed_module.warnings(),
    //     // );
    // }

    // let mut reporter = DiagnosticsReporter::new();
    // reporter.add_all(test_module_id, typed_module.errors().into_iter().cloned());
    //
    // format!(
    //     "{typed_module:#?}\n{parse_errors:#?}\n{}\n",
    //     reporter.render_no_color(&db),
    // )

    format!("{typed_module:#?}\n{parse_errors:#?}")
}

// TODO: continue fixing lowering errors in std lib
// #[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(
        |module_files| {
            let mut db = TestHirTyDatabase::default();

            module_files.iter().for_each(|module_file| {
                db.add_module(
                    module_file.slug(),
                    module_file.path(),
                    module_file.contents(),
                );
            });

            db
        },
        |db, module_file| {
            let path = module_file.path();

            let test_module_id = db.find_module_by_slug(module_file.slug()).expect("");

            let type_map = crate::type_check_module(db, test_module_id);

            let type_inference_warnings = type_map.warnings();
            let type_inference_errors = type_map.errors();

            let mut reporter = DiagnosticsReporter::new();
            reporter.add_all(test_module_id, type_inference_errors.into_iter().cloned());

            assert!(
                type_inference_warnings.is_empty(),
                "file '{path}' contained type inference warnings:\n{:#?}",
                type_inference_warnings,
            );
            assert!(
                type_inference_errors.is_empty(),
                "file '{path}' contained type inference errors:\n{}",
                reporter.render_no_color(db),
            );
        },
    );
}
