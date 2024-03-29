use expect_test::expect_file;
use std::ffi::OsStr;
use std::path::Path;
use std::panic::RefUnwindSafe;
use std::{env, fs};

pub fn run_test_dir(tests_dir: &str, test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{tests_dir}"))
    };

    let mut failed_tests = vec![];

    for entry in fs::read_dir(tests_dir).unwrap() {
        let test_path = entry.unwrap().path().canonicalize().unwrap();

        println!(
            "\n==== RUNNING TEST {:?} ====",
            test_path.file_stem().unwrap()
        );

        let file_name = test_path.file_name().unwrap().to_os_string();

        if test_path.extension() != Some(OsStr::new("test")) {
            continue;
        }

        let did_panic =
            std::panic::catch_unwind(|| {
                let test_content = fs::read_to_string(&test_path).unwrap();
                let (input, _expected) = test_content.split_once("\n===\n").unwrap();

                let result = test_fn(&test_path, input);

                let expected_test_content = format!("{input}\n===\n{result}\n");
                expect_file![test_path].assert_eq(&expected_test_content);
            }).is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:?}",
        failed_tests.len(),
        failed_tests,
    );
}

pub fn run_std_lib_tests(test_fn: impl Fn(&Path, &str)) {
    let std_lib = fs::read_dir("../../std")
        .expect("Something went wrong reading the std lib dir")
        .map(|res| {
            res.expect("Something went wrong reading the directory entry")
                .path()
                .canonicalize()
                .expect("Something went wrong canonicalize-ing the directory entry")
        })
        .collect::<Vec<_>>();

    for path in std_lib {
        let file_name = path.to_str().expect("Expected filename");

        let source = fs::read_to_string(file_name)
            .unwrap_or_else(|_| panic!("Something went wrong reading the file '{:?}'", file_name));

        test_fn(&path, &source);
    }
}
