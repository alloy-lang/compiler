use std::path::Path;

use crate::IndexedModule;
use alloy_ast as ast;

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_test(path, input, false, false, index_source_file)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_test(path, input, false, false, index_repl_line)
    });
}

#[test]
fn repl_line_indexing_errors() {
    alloy_test_harness::run_test_dir("repl_line_indexing_errors", |path, input| {
        run_hir_test(path, input, false, true, index_repl_line)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_test(path, input, true, false, index_repl_line)
    });
}

#[track_caller]
fn index_source_file(input: &str) -> (IndexedModule, Vec<alloy_parser::ParseError>) {
    let parse = alloy_parser::parse_source_file(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let hir = crate::index_source_file(&source_file);
    (hir, parse_errors)
}

#[track_caller]
fn index_repl_line(input: &str) -> (IndexedModule, Vec<alloy_parser::ParseError>) {
    let parse = alloy_parser::parse_source_file(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let hir = crate::index_repl_line(&source_file);
    (hir, parse_errors)
}

#[track_caller]
fn run_hir_test(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    expect_indexing_errors: bool,
    func: fn(&str) -> (IndexedModule, Vec<alloy_parser::ParseError>),
) -> String {
    let (indexed, parse_errors) = func(input);

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
    // if expect_indexing_errors {
    //     assert!(
    //         !indexed.errors().is_empty() || !indexed.warnings().is_empty(),
    //         "file '{}' did not contain indexing errors or warnings",
    //         file_name
    //     );
    // } else {
    //     assert!(
    //         indexed.errors().is_empty(),
    //         "file '{file_name}' contained indexing errors: {:?}",
    //         indexed.errors(),
    //     );
    // }

    format!("{indexed:#?}\n{parse_errors:#?}")
}

#[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let (module, parse_errors) = index_source_file(source);
        let indexing_warnings = module.warnings();
        let indexing_errors = module.errors();

        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:#?}",
            file_name,
            parse_errors,
        );
        assert!(
            indexing_warnings.is_empty(),
            "file '{}' contained indexing warnings: {:#?}",
            file_name,
            indexing_warnings,
        );
        assert!(
            indexing_errors.is_empty(),
            "file '{}' contained indexing errors: {:#?}",
            file_name,
            indexing_errors,
        );
    });
}
