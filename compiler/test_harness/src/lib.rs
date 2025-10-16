use alloy_project::Project;
use expect_test::expect_file;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::path::{Path, PathBuf};
use std::{env, fs};

/// # Panics
///
/// Will panic if tests fail.
#[track_caller]
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

/// # Panics
///
/// Will panic if tests fail.
#[track_caller]
pub fn run_test_dir(
    tests_dir: &str,
    test_fn: impl Fn(&Path, &str) -> String + RefUnwindSafe + UnwindSafe,
) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{tests_dir}"))
    };

    let mut failed_tests = vec![];

    for entry in fs::read_dir(&tests_dir).unwrap() {
        let test_path = entry.unwrap().path().canonicalize().unwrap();

        println!(
            "\n==== RUNNING TEST [{:?}] {:?} ====",
            &tests_dir,
            test_path.file_stem().unwrap()
        );

        let file_name = test_path.file_name().unwrap().to_os_string();

        if test_path.ends_with("test") {
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

    failed_tests.sort();

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}

/// # Panics
///
/// Will panic if tests fail.
#[track_caller]
pub fn run_std_lib_tests(test_fn: impl Fn(&Path, &str) + RefUnwindSafe + UnwindSafe) {
    let project = Project::new("../../std").expect("expected project to be created");

    let mut failed_tests = vec![];
    for module_file in project.modules() {
        let path = module_file.path();

        println!(
            "\n==== RUNNING STD LIB TEST {:?} ====",
            path.file_stem().unwrap()
        );

        let did_panic = std::panic::catch_unwind(|| {
            test_fn(path.as_std_path(), module_file.contents());
        })
            .is_err();

        if did_panic {
            failed_tests.push(path.as_std_path());
        }
    }

    assert!(
        failed_tests.is_empty(),
        "{} test(s) failed: {:#?}",
        failed_tests.len(),
        failed_tests,
    );
}
