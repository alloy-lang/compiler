use std::path::Path;

use crate::hir::HirModule;
use alloy_ast as ast;

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_test(path, input, false, lower_source_file)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_test(path, input, false, lower_repl_line)
    });
}

#[test]
fn repl_line_errors() {
    alloy_test_harness::run_test_dir("repl_line_errors", |path, input| {
        run_hir_test(path, input, true, lower_repl_line)
    });
}

#[track_caller]
fn lower_source_file(input: &str) -> (HirModule, Vec<alloy_parser::ParseError>) {
    let parse = alloy_parser::parse_source_file(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let hir = crate::lower_source_file(&source_file);
    (hir, parse_errors)
}

#[track_caller]
fn lower_repl_line(input: &str) -> (HirModule, Vec<alloy_parser::ParseError>) {
    let parse = alloy_parser::parse_repl_line(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let hir = crate::lower_repl_line(&source_file);
    (hir, parse_errors)
}

#[track_caller]
fn run_hir_test(
    path: &Path,
    input: &str,
    ignore_parse_errors: bool,
    func: fn(&str) -> (HirModule, Vec<alloy_parser::ParseError>),
) -> String {
    let (module, parse_errors) = func(input);

    if !ignore_parse_errors {
        let file_name = path.to_str().expect("Expected filename");
        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:?}",
            file_name,
            parse_errors
        );
    }

    format!("{module:#?}\n{parse_errors:#?}")
}

// TODO: continue fixing lowering errors in std lib
// #[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let (module, parse_errors) = lower_source_file(source);
        let lowering_errors = module.errors();

        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:#?}",
            file_name,
            parse_errors,
        );
        assert!(
            lowering_errors.is_empty(),
            "file '{}' contained lowering errors: {:#?}",
            file_name,
            lowering_errors,
        );
    });
}
