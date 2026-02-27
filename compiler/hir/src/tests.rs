use alloy_workspace::WorkspaceDatabase;
use camino::Utf8Path;
use std::env;
use std::path::Path;

alloy_test_harness::test_database!(TestHirDatabase: crate::HirDatabase);

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_test(path, input, false, false)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_test(path, input, false, false)
    });
}

#[test]
fn repl_line_lowering_errors() {
    alloy_test_harness::run_test_dir("repl_line_lowering_errors", |path, input| {
        run_hir_test(path, input, false, true)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_test(path, input, true, false)
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
                    run_hir_test(path, input, true, false)
                });
            })
            .is_err();

            assert!(!did_panic, "{} test failed", test_case,);

            break;
        }
    }
}

#[track_caller]
fn run_hir_test(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    expect_lowering_errors: bool,
) -> String {
    let mut db = TestHirDatabase::default();

    let module_id = db.add_module("test", Utf8Path::from_path(path).unwrap(), input);
    let (module, parse_errors) = crate::lower_file(&db, module_id);

    let file_name = path.to_str().expect("Expected filename");
    if expect_parse_errors {
        assert!(
            !parse_errors.is_empty(),
            "file '{}' did not contain parse errors",
            file_name
        );
    } else {
        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {parse_errors:?}",
            file_name
        );
    }
    if expect_lowering_errors {
        assert!(
            !module.errors().is_empty() || !module.warnings().is_empty(),
            "file '{}' did not contain lowering errors or warnings",
            file_name
        );
    } else {
        assert!(
            module.errors().is_empty(),
            "file '{file_name}' contained lowering errors: {:#?}",
            module.errors(),
        );
        // TODO: decide if we want to fail tests on warnings
        // assert!(
        //     module.warnings().is_empty(),
        //     "file '{file_name}' contained lowering warnings: {:#?}",
        //     module.warnings(),
        // );
    }

    format!("{:#?}\n{parse_errors:#?}", module)
}

#[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(
        |_module_files| TestHirDatabase::default(),
        |db, module_file| {
            let path = module_file.path();
            let source = module_file.contents();

            let module_id = db.add_module("test", path, source);
            let (module, parse_errors) = crate::lower_file(db, module_id);
            let lowering_warnings = module.warnings();
            let lowering_errors = module.errors();

            assert!(
                parse_errors.is_empty(),
                "file '{path}' contained parse errors: {:#?}",
                parse_errors,
            );
            assert!(
                lowering_warnings.is_empty(),
                "file '{path}' contained lowering warnings: {:#?}",
                lowering_warnings,
            );
            assert!(
                lowering_errors.is_empty(),
                "file '{path}' contained lowering errors: {:#?}",
                lowering_errors,
            );
        },
    );
}
