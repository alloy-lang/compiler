use alloy_parser::Parse;
use expect_test::expect_file;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::{env, fmt, fs};

use crate::ast::AstNode;
use crate::ast::SourceFile;

#[test]
fn source_file() {
    run_parser_tests(
        "source_file",
        alloy_parser::parse_source_file,
        SourceFile::module,
    );
}

#[test]
fn repl_line() {
    run_parser_tests(
        "repl_line",
        alloy_parser::parse_repl_line,
        SourceFile::statements,
    );
}

// #[test]
// fn repl_line_old() {
//     run_parser_tests(
//         "repl_line_old",
//         alloy_parser::parse_repl_line,
//         SourceFile::statements,
//     );
// }

fn run_parser_test<T: fmt::Debug>(
    path: PathBuf,
    parsing_fn: fn(&str) -> Parse,
    thing_fn: fn(&SourceFile) -> T,
) {
    let test_content = fs::read_to_string(&path).unwrap();
    let (input, _expected_parse) = test_content.split_once("\n===\n").unwrap();

    let actual = parsing_fn(input);

    let syntax = actual.syntax();

    {
        let file_name = path.to_str().expect("Expected filename");
        let parse_errors = actual.errors();
        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:?}",
            file_name,
            parse_errors
        );
    }

    let validation_errors = crate::validation::validate(&syntax);
    let source_file = SourceFile::cast(syntax).unwrap();

    let expected_test_content = format!(
        "{}\n===\n{:#?}\n{:#?}\n",
        input,
        thing_fn(&source_file),
        validation_errors
    );

    expect_file![path].assert_eq(&expected_test_content);
}

fn run_parser_tests<T: fmt::Debug>(
    tests_dir: &str,
    parsing_fn: fn(&str) -> Parse,
    thing_fn: fn(&SourceFile) -> T,
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

        let did_panic =
            std::panic::catch_unwind(|| run_parser_test(path, parsing_fn, thing_fn)).is_err();

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
        let actual = alloy_parser::parse_source_file(&source);

        let syntax = actual.syntax();
        let validation_errors = crate::validation::validate(&syntax);
        let source_file = SourceFile::cast(syntax).unwrap();

        assert!(
            validation_errors.is_empty(),
            "file '{}' contained: {:?}",
            file_name,
            source_file,
        );
    }
}
