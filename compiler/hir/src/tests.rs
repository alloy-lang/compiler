use crate::hir::HirModule;
use alloy_ast as ast;
use std::env;
use std::path::Path;

#[salsa::db]
#[derive(Clone, Default)]
pub(crate) struct TestHirDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for TestHirDatabase {}

#[salsa::db]
impl crate::HirDatabase for TestHirDatabase {}

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
fn lower_source_file<'db>(
    db: &'db dyn crate::HirDatabase,
    input: &str,
) -> (HirModule, Vec<alloy_parser::ParseError>) {
    let (source_file, parse_errors) = ast::source_file(input);
    let source_file = source_file.expect("Failed to parse source file");

    let hir = crate::lower_source_file(db, &source_file);
    (hir, parse_errors)
}

#[track_caller]
fn run_hir_test<'db>(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    expect_lowering_errors: bool,
) -> String {
    let db = TestHirDatabase::default();
    let (module, parse_errors) = lower_source_file(&db, input);

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
    }

    format!("{:#?}\n{parse_errors:#?}", module)
}

// TODO: continue fixing lowering errors in std lib
// #[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let db = TestHirDatabase::default();
        let (module, parse_errors) = lower_source_file(&db, source);
        let lowering_warnings = module.warnings();
        let lowering_errors = module.errors();

        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:#?}",
            file_name,
            parse_errors,
        );
        assert!(
            lowering_warnings.is_empty(),
            "file '{}' contained lowering warnings: {:#?}",
            file_name,
            lowering_warnings,
        );
        assert!(
            lowering_errors.is_empty(),
            "file '{}' contained lowering errors: {:#?}",
            file_name,
            lowering_errors,
        );
    });
}
