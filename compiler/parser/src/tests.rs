use crate::Parse;

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |_path, input| {
        run_parser_test(input, crate::parse_source_file)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |_path, input| {
        run_parser_test(input, crate::parse_repl_line)
    });
}

fn run_parser_test(input: &str, parsing_fn: fn(&str) -> Parse) -> String {
    let actual_parse = parsing_fn(input);

    actual_parse.debug_tree().to_string()
}

#[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let actual = crate::parse_source_file(source);

        assert!(
            actual.errors.is_empty(),
            "file '{}' contained: {}",
            file_name,
            actual.debug_tree(),
        );
    });
}
