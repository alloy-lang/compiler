use alloy_hir_def as hir;
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
pub trait HirTyDatabase: hir::HirDefDatabase {}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypedModule {
    module_id: ModuleId,
    expression_types: FxHashMap<hir::ExpressionIdx, ResolvedType>,
    pattern_types: FxHashMap<hir::PatternIdx, ResolvedType>,
    warnings: Vec<TypeInferenceWarning>,
    errors: Vec<TypeInferenceError>,
    /// Track polymorphic instantiations: definition -> list of instantiations
    /// Each instantiation records where the polymorphic value was used and with what concrete types
    poly_instantiations: FxHashMap<EPTdFql, Vec<PolyInstantiation>>,
}

impl HirTypedModule {
    pub(crate) fn empty(module_id: ModuleId) -> Self {
        Self {
            module_id,
            expression_types: HashMap::default(),
            pattern_types: HashMap::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
            poly_instantiations: FxHashMap::default(),
        }
    }

    pub(crate) fn insert_type(&mut self, fql: EPTdFql, resolved_type: ResolvedType) {
        // Only include types for the current module to avoid cross-module collisions
        // (different modules can have the same Idx<Expression> values)
        if fql.module_id() != self.module_id {
            return;
        }

        match fql {
            EPTdFql::Expression(fql) => {
                self.expression_types.insert(fql.local_id, resolved_type);
            }
            EPTdFql::Pattern(fql) => {
                self.pattern_types
                    .insert(fql.local_id, resolved_type.clone());
            }
            EPTdFql::TypeDefinition(_) | EPTdFql::TypeDefinitionVariant(_, _) => {}
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
    use crate::diagnostics::{ConflictingTypeAnnotationReason, TypeInferenceError, TypeInferenceErrorKind};
    use crate::hir_ty::ResolvedType;
    use crate::tests::TestHirTyDatabase;
    use alloy_hir_def as hir;
    use alloy_hir_resolved::{AnnotatedType, EPTdFql, Fql};
    use alloy_scope::Scopes;
    use alloy_test_harness::idx;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use non_empty_vec::NonEmpty;
    use salsa::Database;
    use text_size::{TextRange, TextSize};

    fn check(input: &str, expected: &[(u32, ResolvedType)]) {
        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            input,
        );

        let (_, parse_errors) = hir::lower_file(&db, module_id);

        let ctx = crate::type_check_module(&db, module_id);

        assert_eq!(parse_errors, &[]);
        assert_eq!(ctx.errors, &[]);

        let expected = expected
            .into_iter()
            .map(|(id, ty)| (idx!(*id), ty.clone()))
            .collect();

        db.attach(|_| assert_eq!(ctx.expression_types, expected));
    }

    fn check_named(
        db: &mut TestHirTyDatabase,
        input: &str,
        expected: &[(&str, u32, ResolvedType)],
    ) {
        let module_id = db.add_test_module("test_data", input);

        let (hir_module, parse_errors) = hir::lower_file(db, module_id);
        eprintln!("hir_module = {:#?}", hir_module);
        assert_eq!(parse_errors, &[]);

        let ctx = crate::type_check_module(db, module_id);
        eprintln!("typed module = {:#?}", ctx);
        assert_eq!(ctx.errors, &[]);

        let actual = expected
            .into_iter()
            .map(|(name, scope, _ty)| {
                let (expression_id, _expression) = hir_module
                    .get_expression_by_name(&hir::Name::new(*name), idx!(*scope))
                    .expect("expression not found");
                (*name, *scope, ctx.expression_types[&expression_id].clone())
            })
            .collect::<Vec<_>>();

        db.attach(|_| assert_eq!(actual, expected));
    }

    fn check_expr_instantiations(
        db: &mut TestHirTyDatabase,
        input: &str,
        name: &str,
        expected: &[&[ResolvedType]],
    ) {
        let module_id = db.add_test_module("test_data", input);

        let (hir_module, parse_errors) = hir::lower_file(db, module_id);
        assert_eq!(parse_errors, &[]);

        let (id_expr, _) = hir_module
            .get_expression_by_name(&hir::Name::new(name), Scopes::ROOT)
            .unwrap();
        let id_fql = EPTdFql::Expression(Fql::new(module_id, id_expr));

        check_instantiations(db, module_id, id_fql, name, expected);
    }

    fn check_type_def_instantiations(
        db: &mut TestHirTyDatabase,
        input: &str,
        name: &str,
        expected: &[&[ResolvedType]],
    ) {
        let module_id = db.add_test_module("test_data", input);

        let (hir_module, parse_errors) = hir::lower_file(db, module_id);
        assert_eq!(parse_errors, &[]);

        let (id_expr, _) = hir_module
            .get_type_definition_by_name(&hir::Name::new(name), Scopes::ROOT)
            .unwrap();
        let id_fql =
            EPTdFql::TypeDefinitionVariant(Fql::new(module_id, id_expr), hir::Name::new("Some"));

        check_instantiations(db, module_id, id_fql, name, expected);
    }

    fn check_instantiations(
        db: &mut TestHirTyDatabase,
        module_id: ModuleId,
        fql: EPTdFql,
        name: &str,
        expected: &[&[ResolvedType]],
    ) {
        let ctx = crate::type_check_module(db, module_id);

        assert_eq!(ctx.errors, &[]);

        // Verify id was instantiated twice
        let instantiations = ctx.instantiations(&fql);
        assert_eq!(
            instantiations.len(),
            expected.len(),
            "Expected matching instantiations of {}",
            name
        );

        // Collect the type args
        let type_args: Vec<_> = instantiations.iter().map(|inst| &inst.type_args).collect();

        db.attach(|_| assert_eq!(type_args, expected));
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
    fn salsa_cache_invalidation_type_check_module() {
        let module_path = camino::Utf8Path::new("./test/test.alloy");
        let module_slug = "test";

        let mut db = TestHirTyDatabase::default();
        let module_id = db.add_module(module_slug, module_path, "let x = 1");

        let first = crate::type_check_module(&db, module_id);

        db.add_module(module_slug, module_path, "let x = 1\nlet y = 2");

        let second = crate::type_check_module(&db, module_id);
        assert_ne!(first, second);
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
        let mut db = TestHirTyDatabase::default();
        check_named(
            &mut db,
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
            let mut db = TestHirTyDatabase::default();
            check_named(
                &mut db,
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
        let mut db = TestHirTyDatabase::default();
        check_named(
            &mut db,
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
        let mut db = TestHirTyDatabase::default();
        check_named(
            &mut db,
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
                    annotated_type: AnnotatedType::BuiltIn(hir::BuiltInType::String),
                    inferred_type: ResolvedType::BuiltIn(hir::BuiltInType::Int),
                    reason: ConflictingTypeAnnotationReason::DirectConflict {
                        annotated_type: AnnotatedType::BuiltIn(hir::BuiltInType::String),
                        inferred_type: ResolvedType::BuiltIn(hir::BuiltInType::Int),
                    },
                },
                TextRange::new(TextSize::from(51), TextSize::from(73)),
            )],
        );
    }

    #[test]
    fn type_annotation_hint_at_generic_refinement() {
        let mut db = TestHirTyDatabase::default();
        check_named(
            &mut db,
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
        let mut db = TestHirTyDatabase::default();
        check_named(
            &mut db,
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
        check_expr_instantiations(
            &mut db,
            r#"
                typeof id : t1 -> t1 where
                  typevar t1
                let id = |x| -> x

                let string_result = id("hi")
                let int_result = id(42)
            "#,
            "id",
            &[
                &[ResolvedType::BuiltIn(hir::BuiltInType::Int)],
                &[ResolvedType::BuiltIn(hir::BuiltInType::String)],
            ],
        );
    }

    #[test]
    fn track_polymorphic_type_def_usage() {
        let mut db = TestHirTyDatabase::default();
        check_type_def_instantiations(
            &mut db,
            r#"
                typedef Option[t] =
                  | None
                  | Some(t)
                end

                let example = Option::Some("hello")
            "#,
            "Option",
            &[&[ResolvedType::BuiltIn(hir::BuiltInType::String)]],
        );
    }

    #[test]
    fn track_polymorphic_type_def_usage_indirect() {
        let mut db = TestHirTyDatabase::default();
        check_type_def_instantiations(
            &mut db,
            r#"
                typedef Option[t] =
                  | None
                  | Some(t)
                end

                let constructor_example = Option::Some

                let example = constructor_example("hello")
            "#,
            "Option",
            &[&[ResolvedType::BuiltIn(hir::BuiltInType::String)]],
        );
    }

    /// Cross-module reference to a simple value should resolve its type
    #[test]
    fn infer_cross_module_variable_reference() {
        let mut db = TestHirTyDatabase::default();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            "let value = 42",
        );

        check_named(
            &mut db,
            r"
            import other::value
            let x = value
            ",
            &[("x", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int))],
        );
    }

    /// Cross-module reference inside a lambda body should resolve correctly
    #[test]
    fn infer_cross_module_reference_in_lambda() {
        let mut db = TestHirTyDatabase::default();
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            "let value = 42",
        );

        check_named(
            &mut db,
            r"
            import other::value
            let f = |a| -> value
            ",
            &[(
                "f",
                0,
                ResolvedType::Lambda {
                    arg_type: Box::new(ResolvedType::Generic(0)),
                    return_type: Box::new(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                },
            )],
        );
    }

    /// Cross-module reference to a typedef constructor result should resolve the full type
    #[test]
    fn infer_cross_module_typedef_in_lambda() {
        let mut db = TestHirTyDatabase::default();
        let other_module_id = db.add_module(
            "other",
            camino::Utf8Path::new("./test/other.alloy"),
            r"
            typedef Test[t] = Thing t
            let test = Test(0)
            ",
        );

        check_named(
            &mut db,
            r"
            import other::test
            let f = |a, b| -> test
            ",
            &[(
                "f",
                0,
                ResolvedType::Lambda {
                    arg_type: Box::new(ResolvedType::Generic(1)),
                    return_type: Box::new(ResolvedType::Lambda {
                        arg_type: Box::new(ResolvedType::Generic(2)),
                        return_type: Box::new(ResolvedType::Bounded {
                            base: Box::new(ResolvedType::TypeDef(
                                Fql::new(other_module_id, idx!(0)),
                                hir::Name::new("Test"),
                            )),
                            args: vec![ResolvedType::BuiltIn(hir::BuiltInType::Int)],
                        }),
                    }),
                },
            )],
        );
    }

    /// Test that expression index collision between local and imported module
    /// doesn't cause incorrect type inference (is_in_later_group bug)
    #[test]
    fn cross_module_expr_index_collision_does_not_affect_inference() {
        let mut db = TestHirTyDatabase::default();
        // other module has 3 expressions, so Idx(0), Idx(1), Idx(2)
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r"
            let a = 1
            let b = 2
            let c = 3
            ",
        );

        check_named(
            &mut db,
            r"
            import other::c
            let x = 1
            let y = x
            let z = c
            ",
            &[
                ("x", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                ("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                ("z", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
            ],
        );
    }
}
