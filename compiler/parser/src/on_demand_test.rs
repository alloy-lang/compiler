use crate::Parse;

use std::env;

#[test]
fn on_demand_test() {
    for arg in env::args() {
        if arg.contains("--test-case") {
            let test_case = arg.split("--test-case=").nth(1).unwrap();

            let tests_path = {
                let current_dir = env::current_dir().unwrap();
                current_dir.join(format!("src/tests/{test_case}"))
            };

            let did_panic = std::panic::catch_unwind(|| {
                alloy_test_harness::run_test_case(tests_path, |_path, input| {
                    run_parser_test(input, crate::parse_source_file)
                });
            })
            .is_err();

            assert!(!did_panic, "{} test failed", test_case,);

            break;
        }
    }
}

fn run_parser_test(input: &str, parsing_fn: fn(&str) -> Parse) -> String {
    let actual_parse = parsing_fn(input);

    actual_parse.debug_tree().to_string()
}
