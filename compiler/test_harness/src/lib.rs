use alloy_project::Project;
use expect_test::expect_file;
use std::ffi::OsStr;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::path::{Path, PathBuf};
use std::{env, fs};

pub fn run_test_case(
    test_path: PathBuf,
    test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe + UnwindSafe,
) {
    let test_content = fs::read_to_string(&test_path).unwrap();
    let (input, _expected) = test_content.split_once("\n===\n").unwrap();

    let result = test_fn(&test_path, input);

    let expected_test_content = format!("{input}\n===\n{result}\n");
    expect_file![test_path].assert_eq(&expected_test_content);
}

pub fn run_test_dir(
    tests_dir: &str,
    test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe + UnwindSafe,
) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{tests_dir}"))
    };

    let mut failed_tests = vec![];

    for entry in fs::read_dir(tests_dir).unwrap() {
        let test_path = entry.unwrap().path().canonicalize().unwrap();

        println!(
            "\n==== RUNNING TEST [{}] {:?} ====",
            raw_tests_dir,
            test_path.file_stem().unwrap()
        );

        let file_name = test_path.file_name().unwrap().to_os_string();

        if test_path.extension() != Some(OsStr::new("test")) {
            continue;
        }

        let did_panic = std::panic::catch_unwind(|| {
            run_test_case(test_path, &test_fn);
        })
        .is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}

pub fn run_std_lib_tests(test_fn: impl Fn(&Path, &str)) {
    let project = Project::new(Path::new("../../std")).expect("expected project to be created");

    let mut failed_tests = vec![];
    for (_name, path) in project.modules() {
        let file_name = path.to_str().expect("Expected filename");

        println!(
            "\n==== RUNNING STD LIB TEST {:?} ====",
            path.file_stem().unwrap()
        );

        let source = fs::read_to_string(file_name)
            .unwrap_or_else(|_| panic!("Something went wrong reading the file '{:?}'", file_name));

        let did_panic = std::panic::catch_unwind(|| {
            test_fn(&path, &source);
        })
            .is_err();

        if did_panic {
            failed_tests.push(file_name);
        }
    }

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}
