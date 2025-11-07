use crate::{
    lower_expressions, lower_imports, lower_patterns, lower_type_definitions,
    lower_type_references, Expression, Import, LoweringError, LoweringWarning, Pattern, SourceFile,
    TypeDefinition, TypeReference,
};
use alloy_scope::Scopes;
use salsa::plumbing::DatabaseDownCaster;
use std::path::Path;

#[salsa::db]
#[derive(Clone, Default)]
pub(crate) struct TestHirDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for TestHirDatabase {}

#[salsa::db]
impl crate::HirDatabase for TestHirDatabase {}

#[test]
fn source_file() {
    alloy_test_harness::run_test_dir("source_file", |path, input| {
        run_hir_test(path, input, false, false)
    });
}

#[test]
fn repl_line() {
    alloy_test_harness::run_test_dir("repl_line", |path, input| {
        run_hir_test(path, input, false, false)
    });
}

#[test]
fn repl_line_lowering_errors() {
    alloy_test_harness::run_test_dir("repl_line_lowering_errors", |path, input| {
        run_hir_test(path, input, false, true)
    });
}

#[test]
fn repl_line_parse_errors() {
    alloy_test_harness::run_test_dir("repl_line_parse_errors", |path, input| {
        run_hir_test(path, input, true, false)
    });
}

#[derive(Debug)]
pub struct HirModule<'db> {
    pub imports: Vec<Import<'db>>,
    pub expressions: Vec<Expression>,
    pub patterns: Vec<Pattern>,
    pub type_references: Vec<TypeReference>,
    pub type_definitions: Vec<TypeDefinition>,
    pub scopes: Scopes,
    pub warnings: Vec<LoweringWarning>,
    pub errors: Vec<LoweringError>,
}

#[track_caller]
fn lower_source_file<'db>(
    db: &'db dyn crate::HirDatabase,
    input: &str,
) -> (HirModule<'db>, Vec<alloy_parser::ParseError>) {
    let (_, parse_errors) = alloy_ast::source_file(input);
    let hir = lower_inner(db, input);

    (hir, parse_errors)
}

#[track_caller]
fn lower_inner<'db>(db: &'db dyn crate::HirDatabase, input: &str) -> HirModule<'db> {
    let mut errors = vec![];

    let source_file = SourceFile::new(db, input.to_string());
    let imports = lower_imports(db, source_file);
    errors.append(
        &mut lower_imports::accumulated::<LoweringError>(db, source_file)
            .into_iter()
            .cloned()
            .collect(),
    );

    let expressions = lower_expressions(db, source_file);
    errors.append(
        &mut lower_expressions::accumulated::<LoweringError>(db, source_file)
            .into_iter()
            .cloned()
            .collect(),
    );
    let patterns = lower_patterns(db, source_file);
    errors.append(
        &mut lower_patterns::accumulated::<LoweringError>(db, source_file)
            .into_iter()
            .cloned()
            .collect(),
    );
    let type_references = lower_type_references(db, source_file);
    errors.append(
        &mut lower_type_references::accumulated::<LoweringError>(db, source_file)
            .into_iter()
            .cloned()
            .collect(),
    );
    let type_definitions = lower_type_definitions(db, source_file);
    errors.append(
        &mut lower_type_definitions::accumulated::<LoweringError>(db, source_file)
            .into_iter()
            .cloned()
            .collect(),
    );

    HirModule {
        imports,
        expressions,
        patterns,
        type_references,
        type_definitions,
        scopes: Default::default(),
        warnings: vec![],
        errors,
    }
}

#[track_caller]
fn run_hir_test<'db>(
    path: &Path,
    input: &str,
    expect_parse_errors: bool,
    expect_lowering_errors: bool,
) -> String {
    let db = TestHirDatabase::default();
    let (module, parse_errors) = lower_source_file(&db, input);

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
    if expect_lowering_errors {
        assert!(
            !module.errors.is_empty() || !module.warnings.is_empty(),
            "file '{}' did not contain lowering errors or warnings",
            file_name
        );
    } else {
        assert!(
            module.errors.is_empty(),
            "file '{file_name}' contained lowering errors: {:#?}",
            module.errors,
        );
    }

    format!("{:#?}\n{parse_errors:#?}", module)
}

// TODO: continue fixing lowering errors in std lib
// #[test]
fn test_std_lib() {
    alloy_test_harness::run_std_lib_tests(|path, source| {
        let file_name = path.to_str().expect("Expected filename");

        let db = TestHirDatabase::default();
        let (module, parse_errors) = lower_source_file(&db, source);
        let lowering_warnings = module.warnings;
        let lowering_errors = module.errors;

        assert!(
            parse_errors.is_empty(),
            "file '{}' contained parse errors: {:#?}",
            file_name,
            parse_errors,
        );
        assert!(
            lowering_warnings.is_empty(),
            "file '{}' contained lowering warnings: {:#?}",
            file_name,
            lowering_warnings,
        );
        assert!(
            lowering_errors.is_empty(),
            "file '{}' contained lowering errors: {:#?}",
            file_name,
            lowering_errors,
        );
    });
}
