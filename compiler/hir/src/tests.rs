use expect_test::expect_file;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::{env, fs};

use crate::hir::{HirModule, LoweringError};
use alloy_ast as ast;

#[test]
fn source_file() {
    run_parser_tests("source_file", lower_source_file);
}

#[test]
fn repl_line() {
    run_parser_tests("repl_line", lower_repl_line);
}

#[test]
fn repl_line_errors() {
    run_parser_tests("repl_line_errors", lower_repl_line);
}

#[track_caller]
fn lower_source_file(
    input: &str,
) -> (HirModule, Vec<alloy_parser::ParseError>, Vec<LoweringError>) {
    let parse = alloy_parser::parse_source_file(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let (hir, lowering_errors) = crate::lower_source_file(&source_file);
    (hir, parse_errors, lowering_errors)
}

#[track_caller]
fn lower_repl_line(input: &str) -> (HirModule, Vec<alloy_parser::ParseError>, Vec<LoweringError>) {
    let parse = alloy_parser::parse_repl_line(input);
    let parse_errors = parse.errors().to_vec();
    let root = parse.syntax();
    let source_file = ast::source_file(root).unwrap();

    let (hir, lowering_errors) = crate::lower_repl_line(&source_file);
    (hir, parse_errors, lowering_errors)
}

#[track_caller]
fn run_parser_test(
    path: PathBuf,
    func: fn(&str) -> (HirModule, Vec<alloy_parser::ParseError>, Vec<LoweringError>),
) {
    let test_content = fs::read_to_string(&path).unwrap();
    let (input, _expected_parse) = test_content.split_once("\n===\n").unwrap();

    let (module, parse_errors, lowering_errors) = func(input);

    let expected_test_content = format!(
        "{}\n===\n{:#?}\n{:#?}\n{:#?}\n",
        input, module, parse_errors, lowering_errors,
    );

    expect_file![path].assert_eq(&expected_test_content);
}

#[track_caller]
fn run_parser_tests(
    tests_dir: &str,
    func: fn(&str) -> (HirModule, Vec<alloy_parser::ParseError>, Vec<LoweringError>),
) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{}", tests_dir))
    };

    let mut failed_tests = vec![];

    for file in fs::read_dir(tests_dir).unwrap() {
        let path = file.unwrap().path();
        let file_name = path.file_name().unwrap().to_os_string();

        if path.extension() != Some(OsStr::new("test")) {
            continue;
        }

        let did_panic = std::panic::catch_unwind(|| run_parser_test(path, func)).is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    if !failed_tests.is_empty() {
        panic!(
            "{} parser test(s) failed: {:?}",
            failed_tests.len(),
            failed_tests
        );
    }
}

#[test]
fn test_std_lib() {
    let std_lib = fs::read_dir("../../std")
        .expect("Something went wrong reading the std lib dir")
        .map(|res| {
            res.expect("Something went wrong reading the directory entry")
                .path()
        })
        .collect::<Vec<_>>();

    for path in std_lib {
        let file_name = path.to_str().expect("Expected filename");

        let source = fs::read_to_string(file_name)
            .unwrap_or_else(|_| panic!("Something went wrong reading the file '{:?}'", file_name));

        let (module, parse_errors, lowering_errors) = lower_source_file(&source);

        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:?}",
            file_name,
            module,
        );
        assert!(
            lowering_errors.is_empty(),
            "file '{}' contained lowering errors: {:?}",
            file_name,
            module,
        );
    }
}
