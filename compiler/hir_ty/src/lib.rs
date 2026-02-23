use alloy_hir as hir;
use alloy_hir_resolved::EPTdFql;
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use text_size::TextRange;

mod hir_ty;
use hir_ty::*;

mod diagnostics;
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
    /// Track polymorphic instantiations: definition -> list of instantiations
    /// Each instantiation records where the polymorphic value was used and with what concrete types
    poly_instantiations: FxHashMap<EPTdFql, Vec<PolyInstantiation>>,
}

impl HirTypedModule {
    pub(crate) fn empty() -> Self {
        Self {
            expression_types: HashMap::default(),
            pattern_types: HashMap::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
            poly_instantiations: FxHashMap::default(),
        }
    }

    pub(crate) fn insert_type(&mut self, fql: EPTdFql, resolved_type: ResolvedType) {
        match fql {
            EPTdFql::Expression(fql) => {
                self.expression_types.insert(fql.local_id, resolved_type);
            }
            EPTdFql::Pattern(fql) => {
                self.pattern_types
                    .insert(fql.local_id, resolved_type.clone());
            }
            EPTdFql::TypeDefinition(_) => {}
        }
    }

    fn warning(&mut self, kind: TypeInferenceWarningKind, range: TextRange) {
        self.warnings.push(TypeInferenceWarning::new(kind, range));
    }

    fn error(&mut self, kind: TypeInferenceErrorKind, range: TextRange) {
        self.errors.push(TypeInferenceError::new(kind, range));
    }

    fn push_error(&mut self, err: TypeInferenceError) {
        self.errors.push(err);
    }

    pub fn warnings(&self) -> &[TypeInferenceWarning] {
        &self.warnings
    }

    pub fn errors(&self) -> &[TypeInferenceError] {
        &self.errors
    }

    #[must_use]
    /// Get all instantiations for a polymorphic definition
    pub fn instantiations(&self, def_fql: &EPTdFql) -> &[PolyInstantiation] {
        // TODO: Add deduplication, if needed
        self.poly_instantiations
            .get(def_fql)
            .map_or(&[], |v| v.as_slice())
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
    infer_types(db, module_id)
}

#[cfg(test)]
mod small_tests {
    use crate::diagnostics::{
        ConflictingTypeAnnotationReason, TypeInferenceError, TypeInferenceErrorKind,
    };
    use crate::hir_ty::ResolvedType;
    use crate::tests::TestHirTyDatabase;
    use alloy_ast as ast;
    use alloy_hir as hir;
    use alloy_hir::ExpressionIdx;
    use alloy_hir_resolved::{EPTdFql, Fql};
    use alloy_scope::{ScopeIdx, Scopes};
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
        check(
            "True",
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Bool))],
        );
        check(
            "False",
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Bool))],
        );
        check(
            "let test = True",
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Bool))],
        );
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
                TypeInferenceErrorKind::ConflictingTypeAnnotation {
                    annotated_type: ResolvedType::BuiltIn(hir::BuiltInType::String),
                    inferred_type: ResolvedType::BuiltIn(hir::BuiltInType::Int),
                    reason: ConflictingTypeAnnotationReason::DirectConflict {
                        annotated_type: ResolvedType::BuiltIn(hir::BuiltInType::String),
                        inferred_type: ResolvedType::BuiltIn(hir::BuiltInType::Int),
                    },
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
            )],
        );
    }

    #[test]
    fn let_polymorphism_identity_function() {
        check_named(
            r#"
                typeof id : t1 -> t1 where
                  typevar t1
                let id = |x| -> x

                let string_example = id("hi")
                let int_example = id(10)
            "#,
            &[
                (
                    "id",
                    0,
                    ResolvedType::Lambda {
                        arg_type: Box::new(ResolvedType::Generic(0)),
                        return_type: Box::new(ResolvedType::Generic(0)),
                    },
                ),
                (
                    "string_example",
                    0,
                    ResolvedType::BuiltIn(hir::BuiltInType::String),
                ),
                (
                    "int_example",
                    0,
                    ResolvedType::BuiltIn(hir::BuiltInType::Int),
                ),
            ],
        );
    }

    #[test]
    fn track_polymorphic_instantiation_direct() {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test/test.alloy"),
            r#"
                typeof id : t1 -> t1 where
                  typevar t1
                let id = |x| -> x

                let string_result = id("hi")
                let int_result = id(42)
            "#,
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let ctx = crate::type_check_module(&db, module_id);

        // Get the FQL for 'id'
        let (id_expr, _) = hir_module
            .get_expression_by_name(&hir::Name::new("id"), Scopes::ROOT)
            .unwrap();
        let id_fql = EPTdFql::Expression(Fql::new(module_id, id_expr));

        // Verify id was instantiated twice
        let instantiations = ctx.instantiations(&id_fql);
        assert_eq!(instantiations.len(), 2, "Expected two instantiations of id");

        // Collect the type args
        let type_args: Vec<_> = instantiations
            .iter()
            .map(|inst| &inst.type_args[0])
            .collect();

        assert!(type_args.contains(&&ResolvedType::BuiltIn(hir::BuiltInType::String)));
        assert!(type_args.contains(&&ResolvedType::BuiltIn(hir::BuiltInType::Int)));
    }

    #[test]
    fn track_polymorphic_type_def_usage() {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test/test.alloy"),
            r#"
                typedef Option[t] =
                  | None
                  | Some(t)
                end

                let example = Option::Some("hello")
            "#,
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let ctx = crate::type_check_module(&db, module_id);

        // Get the FQL for 'example'
        let (option_td, _) = hir_module
            .get_type_definition_by_name(&hir::Name::new("Option"), Scopes::ROOT)
            .unwrap();
        let example_fql = EPTdFql::TypeDefinition(Fql::new(module_id, option_td));

        // Verify id was instantiated twice
        let instantiations = ctx.instantiations(&example_fql);
        assert_eq!(
            instantiations.len(),
            1,
            "Expected instantiations of 'Option': {:#?}",
            ctx.poly_instantiations
        );

        // Collect the type args
        let type_args: Vec<_> = instantiations
            .iter()
            .map(|inst| &inst.type_args[0])
            .collect();

        assert!(type_args.contains(&&ResolvedType::BuiltIn(hir::BuiltInType::String)));
    }

    #[test]
    fn track_polymorphic_type_def_usage_indirect() {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test/test.alloy"),
            r#"
                typedef Option[t] =
                  | None
                  | Some(t)
                end

                let constructor_example = Option::Some

                let example = constructor_example("hello")
            "#,
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);
        let ctx = crate::type_check_module(&db, module_id);

        // Get the FQL for 'example'
        let (option_td, _) = hir_module
            .get_type_definition_by_name(&hir::Name::new("Option"), Scopes::ROOT)
            .unwrap();
        let example_fql = EPTdFql::TypeDefinition(Fql::new(module_id, option_td));

        // Verify id was instantiated twice
        let instantiations = ctx.instantiations(&example_fql);
        assert_eq!(
            instantiations.len(),
            1,
            "Expected instantiations of 'Option': {:#?}",
            ctx.poly_instantiations
        );

        // Collect the type args
        let type_args: Vec<_> = instantiations
            .iter()
            .map(|inst| &inst.type_args[0])
            .collect();

        assert!(type_args.contains(&&ResolvedType::BuiltIn(hir::BuiltInType::String)));
    }
}
