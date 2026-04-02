use alloy_hir_def as hir;
use alloy_hir_infer as hir_infer;
use alloy_hir_infer::{DefinitionInferenceResult, InferredType};
use alloy_hir_resolved::{EPTdFql, Fql};
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use text_size::TextRange;

mod diagnostics;
mod validation;

use diagnostics::{
    TypeCheckingError, TypeCheckingErrorKind, TypeCheckingWarning, TypeCheckingWarningKind,
};

#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirTyDatabase: hir::HirDefDatabase + hir_infer::HirInferDatabase {}

#[derive(Clone, PartialEq)]
pub struct HirTypedModule {
    module_id: ModuleId,
    expression_types: FxHashMap<hir::ExpressionIdx, InferredType>,
    pattern_types: FxHashMap<hir::PatternIdx, InferredType>,
    warnings: Vec<TypeCheckingWarning>,
    errors: Vec<TypeCheckingError>,
}

impl fmt::Debug for HirTypedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("HirTypedModule");
        debug_struct.field("module_id", &self.module_id);
        debug_struct.field(
            "expression_types",
            &self.expression_types.iter().collect::<BTreeMap<_, _>>(),
        );
        debug_struct.field(
            "pattern_types",
            &self.pattern_types.iter().collect::<BTreeMap<_, _>>(),
        );
        debug_struct.field("warnings", &self.warnings);
        debug_struct.field("errors", &self.errors);
        debug_struct.field("poly_instantiations", &FxHashMap::<(), ()>::default());

        debug_struct.finish()
    }
}

impl HirTypedModule {
    pub(crate) fn empty(module_id: ModuleId) -> Self {
        Self {
            module_id,
            expression_types: HashMap::default(),
            pattern_types: HashMap::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn insert_type(&mut self, fql: EPTdFql, resolved_type: InferredType) {
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

    fn warning(&mut self, kind: TypeCheckingWarningKind, range: TextRange) {
        self.warnings.push(TypeCheckingWarning::new(kind, range));
    }

    fn error(&mut self, kind: TypeCheckingErrorKind, range: TextRange) {
        self.errors.push(TypeCheckingError::new(kind, range));
    }

    fn extend_errors(&mut self, errs: &[TypeCheckingError]) {
        self.errors.extend_from_slice(errs);
    }

    pub fn warnings(&self) -> &[TypeCheckingWarning] {
        &self.warnings
    }

    pub fn errors(&self) -> &[TypeCheckingError] {
        &self.errors
    }

    pub(crate) fn merge_inference_result(&mut self, def_result: &DefinitionInferenceResult) {
        for (&eid, ty) in &def_result.expression_types {
            self.insert_type(
                EPTdFql::Expression(Fql::new(self.module_id, eid)),
                ty.clone(),
            );
        }
        for (&pid, ty) in &def_result.pattern_types {
            self.insert_type(EPTdFql::Pattern(Fql::new(self.module_id, pid)), ty.clone());
        }
        self.extend_errors(
            &def_result
                .errors
                .iter()
                .map(|err| err.into())
                .collect::<Vec<_>>(),
        );
    }
}

/// type checking for everything in a module
/// stores resolved types for all module symbols, regardless of scope
/// for the LSP implementation, we will want to generate errors and warnings for the current file
/// during full compilation, we will want to generate errors and warnings for all modules
#[salsa::tracked]
pub fn type_check_module(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let mut result = HirTypedModule::empty(module_id);

    for (_, value_def) in hir_module.values() {
        let value_def = hir::module_value_def(db, module_id, value_def.expr_idx).expect("");
        let def_result = hir_infer::infer_body_type(db, value_def);
        result.merge_inference_result(&def_result);
    }

    let expressions_result = hir_infer::infer_expressions(db, module_id);
    result.merge_inference_result(&expressions_result);

    validation::validate_behaviors(db, module_id, &mut result);
    validation::validate_type_annotations(db, module_id, &mut result);

    result
}

#[cfg(test)]
mod hir_ty_small_tests {
    use crate::diagnostics::{
        ConflictingTypeAnnotationReason, TypeCheckingError, TypeCheckingErrorKind,
    };
    use crate::tests::TestHirTyDatabase;
    use alloy_hir_def as hir;
    use alloy_hir_infer::InferredType;
    use alloy_hir_resolved::{AnnotatedType, Fql};
    use alloy_test_harness::idx;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use non_empty_vec::NonEmpty;
    use salsa::Database;
    use text_size::{TextRange, TextSize};

    fn check(input: &str, expected: &[(u32, InferredType)]) {
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
        expected: &[(&str, u32, InferredType)],
    ) {
        let module_id = db.add_test_module("test_data", input);

        let (hir_module, parse_errors) = hir::lower_file(db, module_id);
        assert_eq!(parse_errors, &[]);

        let ctx = crate::type_check_module(db, module_id);

        let actual = expected
            .into_iter()
            .map(|(name, scope, _ty)| {
                let (expression_id, _expression) = hir_module
                    .get_expression_by_name(&hir::Name::new(*name), idx!(*scope))
                    .expect("expression not found");
                (*name, *scope, ctx.expression_types[&expression_id].clone())
            })
            .collect::<Vec<_>>();

        db.attach(|_| {
            assert_eq!(actual, expected);
            assert_eq!(ctx.errors, &[]);
        });
    }

    fn check_error(input: &str, expected: &[TypeCheckingError]) {
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
        check("1", &[(0, InferredType::BuiltIn(hir::BuiltInType::Int))]);
        check(
            "1.1",
            &[(0, InferredType::BuiltIn(hir::BuiltInType::Fraction))],
        );
        check(
            r#""hello""#,
            &[(0, InferredType::BuiltIn(hir::BuiltInType::String))],
        );
        check("'c'", &[(0, InferredType::BuiltIn(hir::BuiltInType::Char))]);
        check(
            "True",
            &[(0, InferredType::BuiltIn(hir::BuiltInType::Bool))],
        );
        check(
            "False",
            &[(0, InferredType::BuiltIn(hir::BuiltInType::Bool))],
        );
        check(
            "let test = True",
            &[(0, InferredType::BuiltIn(hir::BuiltInType::Bool))],
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
            &[("y", 0, InferredType::BuiltIn(hir::BuiltInType::Int))],
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
                    ("x", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
                    ("y", 0, InferredType::BuiltIn(hir::BuiltInType::String)),
                    (
                        "z",
                        0,
                        InferredType::Tuple(NonEmpty::new_unchecked(vec![
                            InferredType::BuiltIn(hir::BuiltInType::Int),
                            InferredType::BuiltIn(hir::BuiltInType::String),
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
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::Generic(0)),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(0)),
                        return_type: Box::new(InferredType::Generic(0)),
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
                    InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(0)),
                        return_type: Box::new(InferredType::Lambda {
                            arg_type: Box::new(InferredType::Generic(0)),
                            return_type: Box::new(InferredType::Generic(0)),
                        }),
                    },
                ),
                ("y", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
            ],
        );
    }

    // #[test]
    // fn conflicting_type_annotation() {
    //     check_error(
    //         r#"
    //             typeof x : String
    //             let x = 1
    //         "#,
    //         &[TypeCheckingError::new(
    //             TypeCheckingErrorKind::ConflictingTypeAnnotation {
    //                 annotated_type: AnnotatedType::BuiltIn(hir::BuiltInType::String),
    //                 inferred_type: InferredType::BuiltIn(hir::BuiltInType::Int),
    //                 reason: ConflictingTypeAnnotationReason::DirectConflict {
    //                     annotated_type: AnnotatedType::BuiltIn(hir::BuiltInType::String),
    //                     inferred_type: InferredType::BuiltIn(hir::BuiltInType::Int),
    //                 },
    //             },
    //             TextRange::new(TextSize::from(51), TextSize::from(73)),
    //         )],
    //     );
    // }

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
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::String)),
                    return_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::String)),
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
                    InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(0)),
                        return_type: Box::new(InferredType::Generic(0)),
                    },
                ),
                (
                    "string_example",
                    0,
                    InferredType::BuiltIn(hir::BuiltInType::String),
                ),
                (
                    "int_example",
                    0,
                    InferredType::BuiltIn(hir::BuiltInType::Int),
                ),
            ],
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
            &[("x", 0, InferredType::BuiltIn(hir::BuiltInType::Int))],
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
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::Generic(0)),
                    return_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::Int)),
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
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::Generic(0)),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(1)),
                        return_type: Box::new(InferredType::Bounded {
                            base: Box::new(InferredType::TypeDef(
                                Fql::new(other_module_id, idx!(0)),
                                hir::Name::new("Test"),
                            )),
                            args: vec![InferredType::BuiltIn(hir::BuiltInType::Int)],
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
                ("x", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
                ("y", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
                ("z", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
            ],
        );
    }

    #[test]
    fn infer_chained_binary_op() {
        let mut db = TestHirTyDatabase::default();
        let stdlib_eq = ModuleId::new(&db, "std::eq");
        let stdlib_order = ModuleId::new(&db, "std::order");
        let eq_constraint = (Fql::new(stdlib_eq, idx!(0)), hir::Name::new("Eq"));
        let constrained_t1 = InferredType::ConstrainedGeneric {
            id: 1,
            constraints: NonEmpty::new(eq_constraint),
        };

        check_named(
            &mut db,
            r"
            import std::function::(<|)
            import std::order::{Ord, Ordering}

            typeof comparing : (t2 -> t1) -> t2 -> t2 -> Ordering where
              typevar t1 = Ord
              typevar t2
            let comparing = |convert, x, y| -> Ord::compare <| convert(x) <| convert(y)
            ",
            &[(
                "comparing",
                0,
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(0)),
                        return_type: Box::new(constrained_t1),
                    }),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(0)),
                        return_type: Box::new(InferredType::Lambda {
                            arg_type: Box::new(InferredType::Generic(0)),
                            return_type: Box::new(InferredType::TypeDef(
                                Fql::new(stdlib_order, idx!(0)),
                                hir::Name::new("Ordering"),
                            )),
                        }),
                    }),
                },
            )],
        );
    }

    #[test]
    fn infer_monad_join() {
        let mut db = TestHirTyDatabase::default();
        let stdlib_monad = ModuleId::new(&db, "std::monad");
        let monad_constraint = (Fql::new(stdlib_monad, idx!(0)), hir::Name::new("Monad"));
        let constrained_m = InferredType::ConstrainedGeneric {
            id: 0,
            constraints: NonEmpty::new(monad_constraint),
        };

        check_named(
            &mut db,
            r"
            import std::monad::Monad
            import std::monad::(>>=)

            typeof join : m[m[t1]] -> m[t1] where
              typevar m = Monad
              typevar t1
            let join = |mm| -> (mm >>= |x| -> x)
            ",
            &[(
                "join",
                0,
                InferredType::Lambda {
                    arg_type: Box::new(InferredType::Bounded {
                        base: Box::new(constrained_m.clone()),
                        args: vec![InferredType::Bounded {
                            base: Box::new(constrained_m.clone()),
                            args: vec![InferredType::Generic(1)],
                        }],
                    }),
                    return_type: Box::new(InferredType::Bounded {
                        base: Box::new(constrained_m),
                        args: vec![InferredType::Generic(1)],
                    }),
                },
            )],
        );
    }
}
