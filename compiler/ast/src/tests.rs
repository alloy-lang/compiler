use std::fmt;
use std::path::Path;

use crate::ast::AstElement;
use crate::ast::SourceFile;
use alloy_parser::Parse;

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_ast_test(
            path,
            input,
            alloy_parser::parse_source_file,
            SourceFile::module,
        )
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_ast_test(
            path,
            input,
            alloy_parser::parse_repl_line,
            SourceFile::statements,
        )
    });
}

fn run_ast_test<T: fmt::Debug>(
    path: &Path,
    input: &str,
    parsing_fn: fn(&str) -> Parse,
    thing_fn: fn(&SourceFile) -> T,
) -> String {
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

    // todo: ast validation was moved to hir lowering
    format!("{:#?}\n{:#?}", thing_fn(source_file), Vec::<String>::new())
}

#[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let actual = alloy_parser::parse_source_file(source);

        let did_panic = std::panic::catch_unwind(|| {
            let syntax = actual.syntax();
            let _ = SourceFile::cast(syntax).unwrap();
        })
        .is_err();

        assert!(
            !did_panic,
            "file '{}' failed to parse as a SourceFile",
            file_name,
        );
    });
}
