use crate::Parse;
use expect_test::expect_file;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::{env, fs};

// #[test]
// fn source_file() {
//     run_parser_tests("source_file", crate::parse_source_file);
// }

#[test]
fn repl_line() {
    run_parser_tests("repl_line", crate::parse);
}

fn run_parser_test(path: PathBuf, parsing_fn: fn(&str) -> Parse) {
    let test_content = fs::read_to_string(&path).unwrap();
    let (input, _expected_parse) = test_content.split_once("\n===\n").unwrap();

    let actual_parse = parsing_fn(input);

    let expected_test_content = format!("{}\n===\n{}\n", input, actual_parse.debug_tree());

    expect_file![path].assert_eq(&expected_test_content);
}

fn run_parser_tests(tests_dir: &str, parsing_fn: fn(&str) -> Parse) {
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

        let did_panic = std::panic::catch_unwind(|| run_parser_test(path, parsing_fn)).is_err();

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
        let actual = crate::parse(&source);

        assert!(
            actual.errors.is_empty(),
            "file '{}' contained: {}",
            file_name,
            actual.debug_tree(),
        );
    }
}
