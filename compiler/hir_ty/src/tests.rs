use std::path::Path;

use alloy_ast as ast;
use alloy_hir as hir;
use alloy_parser::ParseError;
use alloy_workspace::{ModuleId, SourceFile, Workspace};

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestHirTyDatabase {
    storage: salsa::Storage<Self>,
    workspace: Workspace,
}

#[salsa::db]
impl salsa::Database for TestHirTyDatabase {}

#[salsa::db]
impl alloy_workspace::WorkspaceDatabase for TestHirTyDatabase {
    fn add_module(&mut self, slug: &str, path: &camino::Utf8Path, contents: &str) -> ModuleId {
        self.workspace.add_module(self, slug, path, contents)
    }

    fn get_source(&'_ self, module_id: ModuleId) -> SourceFile<'_> {
        self.workspace.get_source(module_id)
    }
}

#[salsa::db]
impl hir::HirDatabase for TestHirTyDatabase {}

#[salsa::db]
impl crate::HirTyDatabase for TestHirTyDatabase {}

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_ty_test(path, input, false, false, infer_types_source_file)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_ty_test(path, input, false, false, infer_types_repl_line)
    });
}

#[test]
fn repl_line_lowering_errors() {
    alloy_test_harness::run_test_dir("repl_line_lowering_errors", |path, input| {
        run_hir_ty_test(path, input, false, true, infer_types_repl_line)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_ty_test(path, input, true, false, infer_types_repl_line)
    });
}

#[track_caller]
pub(crate) fn infer_types_source_file(
    input: &str,
) -> (crate::InferenceResult, hir::HirModule, Vec<ParseError>) {
    let db = TestHirTyDatabase::default();
    let (source_file, parse_errors) = ast::source_file(input);
    let source_file = source_file.expect("Failed to parse source file");

    let hir = hir::lower_source_file(&db, &source_file);
    (crate::infer_types(&hir), hir, parse_errors)
}

#[track_caller]
pub(crate) fn infer_types_repl_line(
    input: &str,
) -> (crate::InferenceResult, hir::HirModule, Vec<ParseError>) {
    let db = TestHirTyDatabase::default();
    let (source_file, parse_errors) = ast::source_file(input);
    let source_file = source_file.expect("Failed to parse source file");

    let hir = hir::lower_source_file(&db, &source_file);
    (crate::infer_types(&hir), hir, parse_errors)
}

#[track_caller]
fn run_hir_ty_test(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    _expect_lowering_errors: bool,
    func: fn(&str) -> (crate::InferenceResult, hir::HirModule, Vec<ParseError>),
) -> String {
    let (type_map, _hir_module, parse_errors) = func(input);

    let file_name = path.to_str().expect("Expected filename");
    if expect_parse_errors {
        assert!(
            !parse_errors.is_empty(),
            "file '{}' did not contain parse errors",
            file_name
        );
    } else {
        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {parse_errors:?}",
            file_name
        );
    }
    // if expect_lowering_errors {
    //     assert!(
    //         !module.errors().is_empty() || !module.warnings().is_empty(),
    //         "file '{}' did not contain lowering errors or warnings",
    //         file_name
    //     );
    // } else {
    //     assert!(
    //         module.errors().is_empty(),
    //         "file '{file_name}' contained lowering errors: {:?}",
    //         module.errors(),
    //     );
    // }

    format!("{type_map:#?}\n{parse_errors:#?}")
}

// #[test]
// fn test_std_lib() {
//     alloy_test_harness::run_std_lib_tests(|path, source| {
//         let file_name = path.to_str().expect("Expected filename");
//
//         let (module, parse_errors) = lower_source_file(source);
//         let lowering_warnings = module.warnings();
//         let lowering_errors = module.errors();
//
//         assert!(
//             parse_errors.is_empty(),
//             "file '{}' contained parse errors: {:#?}",
//             file_name,
//             parse_errors,
//         );
//         assert!(
//             lowering_warnings.is_empty(),
//             "file '{}' contained lowering warnings: {:#?}",
//             file_name,
//             lowering_warnings,
//         );
//         assert!(
//             lowering_errors.is_empty(),
//             "file '{}' contained lowering errors: {:#?}",
//             file_name,
//             lowering_errors,
//         );
//     });
// }
