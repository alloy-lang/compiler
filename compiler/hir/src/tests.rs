use salsa::DebugWithDb;
use std::path::Path;

use crate::hir::HirModule;
use crate::AstSourceFile;
use alloy_ast as ast;

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub(crate) struct TestHirDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestHirDatabase {}

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_test(path, input, false, false, lower_source_file)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_test(path, input, false, false, lower_source_file)
    });
}

#[test]
fn repl_line_lowering_errors() {
    alloy_test_harness::run_test_dir("repl_line_lowering_errors", |path, input| {
        run_hir_test(path, input, false, true, lower_source_file)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_test(path, input, true, false, lower_source_file)
    });
}

#[track_caller]
fn lower_source_file(input: &str) -> (TestHirDatabase, HirModule, Vec<alloy_parser::ParseError>) {
    let (source_file, parse_errors) = ast::source_file(input);
    let source_file = source_file.expect("Failed to parse source file");

    let db = TestHirDatabase::default();
    let source_file = AstSourceFile::new(&db, source_file.into());
    let hir = crate::lower(&db, source_file);
    (db, hir, parse_errors)
}

#[track_caller]
fn run_hir_test(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    expect_lowering_errors: bool,
    func: fn(&str) -> (TestHirDatabase, HirModule, Vec<alloy_parser::ParseError>),
) -> String {
    let (db, module, parse_errors) = func(input);

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
            !module.errors(&db).is_empty() || !module.warnings(&db).is_empty(),
            "file '{}' did not contain lowering errors or warnings",
            file_name
        );
    } else {
        assert!(
            module.errors(&db).is_empty(),
            "file '{file_name}' contained lowering errors: {:#?}",
            module.errors(&db),
        );
    }

    format!("{:#?}\n{parse_errors:#?}", module.debug(&db))
}

// TODO: continue fixing lowering errors in std lib
// #[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let (db, module, parse_errors) = lower_source_file(source);
        let lowering_warnings = module.warnings(&db);
        let lowering_errors = module.errors(&db);

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
