use std::fmt;
use std::path::Path;

use crate::ast::AstElement;
use crate::ast::SourceFile;

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_ast_test(path, input, SourceFile::module)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_ast_test(path, input, SourceFile::statements)
    });
}

fn run_ast_test<T: fmt::Debug>(path: &Path, input: &str, thing_fn: fn(&SourceFile) -> T) -> String {
    let (source_file, parse_errors) = crate::source_file(input);
    let source_file = source_file.expect("Failed to parse source file");

    {
        let file_name = path.to_str().expect("Expected filename");
        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:?}",
            file_name,
            parse_errors
        );
    }

    // todo: ast validation was moved to hir lowering
    format!("{:#?}\n{:#?}", thing_fn(&source_file), Vec::<String>::new())
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
