use alloy_hir as hir;
use alloy_hir::{Expression, Pattern};
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;
use text_size::TextRange;

mod hir_ty;
use hir_ty::*;

mod diagnostics;
mod resolution;
use diagnostics::*;

#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirTyDatabase: hir::HirDatabase {}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypedModule {
    expression_types: FxHashMap<hir::ExpressionIdx, ResolvedType>,
    pattern_types: FxHashMap<hir::PatternIdx, ResolvedType>,
    warnings: Vec<TypeInferenceWarning>,
    errors: Vec<TypeInferenceError>,
}

impl HirTypedModule {
    pub(crate) fn empty() -> Self {
        Self {
            expression_types: Default::default(),
            pattern_types: Default::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn warning(&mut self, kind: TypeInferenceWarningKind, range: TextRange) {
        self.warnings.push(TypeInferenceWarning::new(kind, range));
    }

    fn error(&mut self, kind: TypeInferenceErrorKind, range: TextRange) {
        self.errors.push(TypeInferenceError::new(kind, range));
    }

    fn warnings(&self) -> &[TypeInferenceWarning] {
        &self.warnings
    }

    fn errors(&self) -> &[TypeInferenceError] {
        &self.errors
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResolutionResult {
    resolved_type: Option<ResolvedType>,
    warnings: Vec<TypeInferenceWarning>,
    errors: Vec<TypeInferenceError>,
}

/// type checking for everything in a module
/// stores resolved types for all module symbols, regardless of scope
/// for the LSP implementation, we will want to generate errors and warnings for the current file
/// during full compilation, we will want to generate errors and warnings for all modules
#[salsa::tracked]
pub fn type_check_module(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    hir_ty::infer_types_hm(db, module_id)
}

/// find an expression's type in a module
#[salsa::tracked]
pub fn find_expression_type(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
) -> TypeResolutionResult {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expression = hir_module.get_expression(expression_id);
    match expression {
        Expression::Missing => {}
        Expression::Literal(_) => {}
        Expression::VariableRef { .. } => {}
        Expression::Binary { .. } => {}
        Expression::Unit => {}
        Expression::IfThenElse { .. } => {}
        Expression::Tuple(_) => {}
        Expression::Unary { .. } => {}
        Expression::Lambda { .. } => {}
        Expression::FunctionCall { .. } => {}
        Expression::Match { .. } => {}
    }

    todo!()
}

#[salsa::tracked]
pub fn find_pattern_type(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    pattern_id: hir::PatternIdx,
) -> TypeResolutionResult {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let pattern = hir_module.get_pattern(pattern_id);
    match pattern {
        Pattern::Missing => {}
        Pattern::Literal(_) => {}
        Pattern::PatternRef { .. } => {}
        Pattern::VariableDeclaration { .. } => {}
        Pattern::Nil => {}
        Pattern::Destructure { .. } => {}
        Pattern::Unit => {}
        Pattern::Tuple(_) => {}
    }

    todo!()
}

#[cfg(test)]
mod small_tests {
    use crate::diagnostics::TypeInferenceError;
    use crate::hir_ty::ResolvedType;
    use crate::tests::TestHirTyDatabase;
    use alloy_ast as ast;
    use alloy_hir as hir;
    use alloy_hir::ExpressionIdx;
    use alloy_scope::ScopeIdx;
    use alloy_workspace::WorkspaceDatabase;
    use la_arena::RawIdx;
    use non_empty_vec::NonEmpty;
    use text_size::{TextRange, TextSize};

    fn check(input: &str, expected: &[(u32, ResolvedType)]) {
        let (_, parse_errors) = ast::source_file(input);

        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            input,
        );

        let ctx = crate::type_check_module(&db, module_id);

        assert_eq!(parse_errors, &[]);
        assert_eq!(ctx.errors, &[]);

        let expected = expected
            .into_iter()
            .map(|(id, ty)| (ExpressionIdx::from_raw(RawIdx::from(*id)), ty.clone()))
            .collect();

        assert_eq!(ctx.expression_types, expected);
    }

    fn check_named(input: &str, expected: &[(&str, u32, ResolvedType)]) {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            input,
        );

        let (hir_module, parse_errors) = hir::lower_file(&db, module_id);
        let ctx = crate::type_check_module(&db, module_id);

        assert_eq!(parse_errors, &[]);
        assert_eq!(ctx.errors, &[]);

        let actual = expected
            .into_iter()
            .map(|(name, scope, _ty)| {
                let (expression_id, _expression) = hir_module
                    .get_expression_by_name(
                        &hir::Name::new(*name),
                        ScopeIdx::from_raw(RawIdx::from(*scope)),
                    )
                    .expect("expression not found");
                (*name, *scope, ctx.expression_types[&expression_id].clone())
            })
            .collect::<Vec<_>>();

        assert_eq!(actual, expected);
    }

    fn check_error(input: &str, expected: &[TypeInferenceError]) {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            input,
        );

        let (_, parse_errors) = hir::lower_file(&db, module_id);
        let ctx = crate::type_check_module(&db, module_id);

        assert_eq!(parse_errors, &[]);

        assert_eq!(ctx.errors, expected);
    }

    #[test]
    fn infer_literals() {
        check("1", &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Int))]);
        check(
            "1.1",
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Fraction))],
        );
        check(
            r#""hello""#,
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::String))],
        );
        check("'c'", &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Char))]);
        check("True", &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Bool))]);
        check("False", &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Bool))]);
    }

    #[test]
    fn infer_variable_ref_literal() {
        check_named(
            r"
                let x = 1
                let y = x
            ",
            &[("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int))],
        );
    }

    #[test]
    fn infer_variable_ref_tuple() {
        unsafe {
            check_named(
                r#"
                let x = 1
                let y = "a"
                let z = (x, y)
            "#,
                &[
                    ("x", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                    ("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::String)),
                    (
                        "z",
                        0,
                        ResolvedType::Tuple(NonEmpty::new_unchecked(vec![
                            ResolvedType::BuiltIn(hir::BuiltInType::Int),
                            ResolvedType::BuiltIn(hir::BuiltInType::String),
                        ])),
                    ),
                ],
            );
        }
    }

    #[test]
    fn infer_variable_ref_unused_lambda() {
        check_named(
            "let x = |a, b| -> a + b",
            &[(
                "x",
                0,
                ResolvedType::Lambda {
                    arg_type: Box::new(ResolvedType::Generic(0)),
                    return_type: Box::new(ResolvedType::Lambda {
                        arg_type: Box::new(ResolvedType::Generic(0)),
                        return_type: Box::new(ResolvedType::Generic(0)),
                    }),
                },
            )],
        );
    }

    #[test]
    fn infer_lambda_based_on_usage() {
        check_named(
            r#"
            let x = |a, b| -> a + b
            let y = x(1, 2)
            "#,
            &[
                (
                    "x",
                    0,
                    ResolvedType::Lambda {
                        arg_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                        return_type: Box::new(ResolvedType::Lambda {
                            arg_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                            return_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                        }),
                    },
                ),
                ("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
            ],
        );
    }

    #[test]
    fn conflicting_type_annotation() {
        check_error(
            r#"
                typeof x : String
                let x = 1
            "#,
            &[TypeInferenceError::new(
                crate::diagnostics::TypeInferenceErrorKind::ConflictingTypeAnnotation {
                    expected: ResolvedType::BuiltIn(hir::BuiltInType::String),
                    found: ResolvedType::BuiltIn(hir::BuiltInType::Int),
                },
                TextRange::new(TextSize::from(51), TextSize::from(73)),
            )],
        );
    }

    #[test]
    fn type_annotation_hint_at_generic_refinement() {
        check_named(
            r#"
                typeof x : String -> String
                let x = |s| -> ""
            "#,
            &[(
                "x",
                0,
                ResolvedType::Lambda {
                    arg_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::String)),
                    return_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::String)),
                },
            )]
        );
    }
}
