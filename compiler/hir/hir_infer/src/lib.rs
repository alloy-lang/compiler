use alloy_hir_def as hir;
use alloy_hir_resolved::EPTdFql;
use alloy_workspace::ModuleId;
use rustc_hash::FxHashMap;
use std::collections::BTreeMap;

mod hir_ty;
pub use hir_ty::{DisplayName, InferredType};

mod diagnostics;
pub use diagnostics::{TypeInferenceError, TypeInferenceWarning};

#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirInferDatabase: hir::HirDefDatabase {}

#[derive(Clone, PartialEq)]
pub struct DefinitionInferenceResult {
    pub expression_types: FxHashMap<hir::ExpressionIdx, InferredType>,
    pub pattern_types: FxHashMap<hir::PatternIdx, InferredType>,
    pub variant_constructor_types:
        FxHashMap<(hir::TypeDefinitionIdx, Option<hir::Name>), InferredType>,
    pub warnings: Vec<TypeInferenceWarning>,
    pub errors: Vec<TypeInferenceError>,
}

impl std::fmt::Debug for DefinitionInferenceResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DefinitionInferenceResult")
            .field("definition_type", &InferredType::Unconstrained)
            .field(
                "expression_types",
                &self.expression_types.iter().collect::<BTreeMap<_, _>>(),
            )
            .field(
                "pattern_types",
                &self.pattern_types.iter().collect::<BTreeMap<_, _>>(),
            )
            .field(
                "variant_constructor_types",
                &self
                    .variant_constructor_types
                    .iter()
                    .collect::<BTreeMap<_, _>>(),
            )
            .field("warnings", &self.warnings)
            .field("errors", &self.errors)
            .finish()
    }
}

impl DefinitionInferenceResult {
    pub(crate) fn empty() -> Self {
        Self {
            expression_types: FxHashMap::default(),
            pattern_types: FxHashMap::default(),
            variant_constructor_types: FxHashMap::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn insert_type(&mut self, fql: impl Into<EPTdFql>, ty: InferredType) {
        match fql.into() {
            EPTdFql::Expression(e) => {
                self.expression_types.insert(e.local_id, ty);
            }
            EPTdFql::Pattern(p) => {
                self.pattern_types.insert(p.local_id, ty);
            }
            EPTdFql::TypeDefinition(td) => {
                self.variant_constructor_types
                    .insert((td.local_id, None), ty);
            }
            EPTdFql::TypeDefinitionVariant(td, name) => {
                self.variant_constructor_types
                    .insert((td.local_id, Some(name)), ty);
            }
        }
    }

    pub(crate) fn extend_errors(&mut self, errs: &[TypeInferenceError]) {
        self.errors.extend_from_slice(errs);
    }

    #[cfg(test)]
    pub(crate) fn compose(mut self, other: Self) -> Self {
        self.expression_types.extend(other.expression_types);
        self.pattern_types.extend(other.pattern_types);
        self.variant_constructor_types
            .extend(other.variant_constructor_types);
        self.warnings.extend(other.warnings);
        self.errors.extend(other.errors);
        self
    }

    pub fn warnings(&self) -> &[TypeInferenceWarning] {
        &self.warnings
    }

    pub fn errors(&self) -> &[TypeInferenceError] {
        &self.errors
    }
}

pub fn infer_body_type(
    db: &'_ dyn HirInferDatabase,
    value_def: hir::ValueDef<'_>,
) -> DefinitionInferenceResult {
    hir_ty::infer_body_type(db, value_def)
}

pub fn infer_expressions(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
) -> DefinitionInferenceResult {
    hir_ty::infer_expressions(db, module_id)
}

#[cfg(test)]
mod hir_infer_small_tests {
    use crate::hir_ty::{DisplayName, InferredType};
    use crate::tests::{infer_module, TestHirInferDatabase};
    use alloy_hir_def as hir;
    use alloy_hir_resolved::{Fql, TraitConstraint};
    use alloy_test_harness::idx;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use non_empty_vec::{ne_vec, NonEmpty};
    use salsa::Database;

    fn check(input: &str, expected: &[(u32, InferredType)]) {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            input,
        );

        let (_, parse_errors) = hir::lower_file(&db, module_id);

        let res = infer_module(&db, module_id);

        assert_eq!(parse_errors, &[]);
        assert_eq!(res.errors, &[]);

        let expected = expected
            .into_iter()
            .map(|(id, ty)| (idx!(*id), ty.clone()))
            .collect();

        db.attach(|_| assert_eq!(res.expression_types, expected));
    }

    fn check_named(
        db: &mut TestHirInferDatabase,
        input: &str,
        expected: &[(&str, u32, InferredType)],
    ) {
        let module_id = db.add_test_module("test_data", input);

        let (hir_module, parse_errors) = hir::lower_file(db, module_id);
        assert_eq!(parse_errors, &[]);

        let res = infer_module(db, module_id);

        let actual = expected
            .into_iter()
            .map(|(name, scope, _ty)| {
                let (expression_id, _expression) = hir_module
                    .get_expression_by_name(&hir::Name::new(*name), idx!(*scope))
                    .expect("expression not found");
                (*name, *scope, res.expression_types[&expression_id].clone())
            })
            .collect::<Vec<_>>();

        db.attach(|_| {
            assert_eq!(actual, expected);
            assert_eq!(res.errors, &[]);
        });
    }

    #[test]
    fn salsa_cache_invalidation_infer_types_module() {
        let module_path = camino::Utf8Path::new("./test/test.alloy");
        let module_slug = "test";

        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_module(module_slug, module_path, "let x = 1");

        let first = infer_module(&db, module_id);

        db.add_module(module_slug, module_path, "let x = 1\nlet y = 2");

        let second = infer_module(&db, module_id);
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
        let mut db = TestHirInferDatabase::default();
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
            let mut db = TestHirInferDatabase::default();
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
        let mut db = TestHirInferDatabase::default();
        let type_var = InferredType::Generic(0, DisplayName::new("a0"));
        check_named(
            &mut db,
            "let x = |a, b| -> a + b",
            &[(
                "x",
                0,
                InferredType::Lambda {
                    arg_type: Box::new(type_var.clone()),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(type_var.clone()),
                        return_type: Box::new(type_var),
                    }),
                },
            )],
        );
    }

    #[test]
    fn infer_lambda_based_on_usage() {
        let mut db = TestHirInferDatabase::default();
        // With per-definition inference, x's type is its principal type from its
        // body alone (generic). y's call-site constraints don't refine x.
        let type_var = InferredType::Generic(0, DisplayName::new("a0"));
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
                        arg_type: Box::new(type_var.clone()),
                        return_type: Box::new(InferredType::Lambda {
                            arg_type: Box::new(type_var.clone()),
                            return_type: Box::new(type_var.clone()),
                        }),
                    },
                ),
                ("y", 0, InferredType::BuiltIn(hir::BuiltInType::Int)),
            ],
        );
    }

    #[test]
    fn type_annotation_hint_at_generic_refinement() {
        let mut db = TestHirInferDatabase::default();
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
        let mut db = TestHirInferDatabase::default();
        let type_var_t1 = InferredType::Generic(0, DisplayName::new("t1"));
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
                        arg_type: Box::new(type_var_t1.clone()),
                        return_type: Box::new(type_var_t1.clone()),
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
        let mut db = TestHirInferDatabase::default();
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
        let mut db = TestHirInferDatabase::default();
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
                    arg_type: Box::new(InferredType::Generic(0, DisplayName::new("a0"))),
                    return_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::Int)),
                },
            )],
        );
    }

    /// Cross-module reference to a typedef constructor result should resolve the full type
    #[test]
    fn infer_cross_module_typedef_in_lambda() {
        let mut db = TestHirInferDatabase::default();
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
                    arg_type: Box::new(InferredType::Generic(0, DisplayName::new("a0"))),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(InferredType::Generic(1, DisplayName::new("b0"))),
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
        let mut db = TestHirInferDatabase::default();
        // other module has 3 expressions, so Idx(0), Idx(1), Idx(2)
        db.add_module(
            "other",
            camino::Utf8Path::new("./other.alloy"),
            r#"
            let a = "hello"
            let b = "there"
            let c = 3
            "#,
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
    fn match_data_destructure_constrains_field_type() {
        let mut db = TestHirInferDatabase::default();
        check_named(
            &mut db,
            r"
            typedef Identity[t] = Identity t

            let boxed = Identity(42)

            let result = match boxed when
              \ Identity(x) -> x
            ",
            &[("result", 0, InferredType::BuiltIn(hir::BuiltInType::Int))],
        );
    }

    #[test]
    fn infer_chained_binary_op() {
        let mut db = TestHirInferDatabase::default();
        let stdlib_eq = ModuleId::new(&db, "std::eq");
        let stdlib_order = ModuleId::new(&db, "std::order");
        let ord_constraint = TraitConstraint {
            trait_fql: Fql::new(stdlib_order, idx!(0)),
            trait_fql_name: "std::order::Ord".to_string(),
            type_var_constraint_fql: Fql::new(stdlib_order, idx!(1)),
        };
        let eq_constraint = TraitConstraint {
            trait_fql: Fql::new(stdlib_eq, idx!(0)),
            trait_fql_name: "std::eq::Eq".to_string(),
            type_var_constraint_fql: Fql::new(stdlib_order, idx!(0)),
        };
        let constrained_t1 = InferredType::ConstrainedGeneric {
            id: 0,
            name: DisplayName::new("t1"),
            constraints: ne_vec![eq_constraint, ord_constraint],
        };

        // TODO: new test, should fail if t1 doesn't constrain on Ord
        let type_var_t2 = InferredType::Generic(1, DisplayName::new("t2"));
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
                        arg_type: Box::new(type_var_t2.clone()),
                        return_type: Box::new(constrained_t1),
                    }),
                    return_type: Box::new(InferredType::Lambda {
                        arg_type: Box::new(type_var_t2.clone()),
                        return_type: Box::new(InferredType::Lambda {
                            arg_type: Box::new(type_var_t2.clone()),
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
        let mut db = TestHirInferDatabase::default();
        let stdlib_monad = ModuleId::new(&db, "std::monad");
        let stdlib_applicative = ModuleId::new(&db, "std::applicative");
        let stdlib_functor = ModuleId::new(&db, "std::functor");
        let monad_constraint = TraitConstraint {
            trait_fql: Fql::new(stdlib_monad, idx!(0)),
            trait_fql_name: "std::monad::Monad".to_string(),
            type_var_constraint_fql: Fql::new(stdlib_monad, idx!(4)),
        };
        let applicative_constraint = TraitConstraint {
            trait_fql: Fql::new(stdlib_applicative, idx!(0)),
            trait_fql_name: "std::applicative::Applicative".to_string(),
            type_var_constraint_fql: Fql::new(stdlib_monad, idx!(4)),
        };
        let functor_constraint = TraitConstraint {
            trait_fql: Fql::new(stdlib_functor, idx!(0)),
            trait_fql_name: "std::functor::Functor".to_string(),
            type_var_constraint_fql: Fql::new(stdlib_monad, idx!(4)),
        };
        let constrained_m = InferredType::ConstrainedGeneric {
            id: 0,
            name: DisplayName::new("m"),
            constraints: ne_vec![monad_constraint, applicative_constraint, functor_constraint],
        };

        let type_var_t1 = InferredType::Generic(1, DisplayName::new("t1"));
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
                            args: vec![type_var_t1.clone()],
                        }],
                    }),
                    return_type: Box::new(InferredType::Bounded {
                        base: Box::new(constrained_m),
                        args: vec![type_var_t1],
                    }),
                },
            )],
        );
    }
}
