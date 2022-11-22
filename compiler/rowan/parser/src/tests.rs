use crate::Parse;
use expect_test::expect_file;
use std::ffi::OsStr;
use std::{env, fs};

// #[test]
// fn source_file() {
//     run_parser_tests("source_file", crate::parse_source_file);
// }

#[test]
fn repl_line() {
    run_parser_tests("repl_line", crate::parse);
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

        let did_panic = std::panic::catch_unwind(|| {
            let test_content = fs::read_to_string(&path).unwrap();
            let (input, _expected_parse) = test_content.split_once("\n===\n").unwrap();

            let actual_parse = parsing_fn(input);

            let expected_test_content = format!("{}\n===\n{}\n", input, actual_parse.debug_tree());

            expect_file![path].assert_eq(&expected_test_content);
        })
        .is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    if !failed_tests.is_empty() {
        panic!("{} parser test(s) failed: {:?}", failed_tests.len(), failed_tests);
    }
}
