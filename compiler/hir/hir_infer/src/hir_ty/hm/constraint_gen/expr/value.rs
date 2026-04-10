use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, AnnotatedType, AnnotatedTypeVar};
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;
use std::convert::TryFrom;

use super::super::super::infer_body_type;
use crate::hir_ty::{DisplayName, InferredType};
use crate::HirInferDatabase;

/// Infer the type signature of a value definition.
///
/// For definitions with type annotations, the signature is derived directly
/// from the annotation. For unannotated definitions, the type is inferred
/// from the body using HM type inference.
///
/// This is a Salsa tracked query with field-level tracking on ValueDef:
/// - Annotated definitions: only re-runs when `type_annotation` changes
/// - Unannotated definitions: re-runs when `expression_idx` changes
#[salsa::tracked(cycle_initial = infer_value_signature_cycle_initial)]
pub fn infer<'db>(db: &'db dyn HirInferDatabase, value_def: hir::ValueDef<'db>) -> InferredType {
    let module_id = value_def.module_id(db);

    // If there's a type annotation, derive the signature from it
    if let Some(type_annotation) = value_def.type_annotation(db) {
        let annotated = resolve_annotated_type(db, module_id, type_annotation);
        if !matches!(annotated, AnnotatedType::Missing) {
            return annotated_to_inferred(&annotated);
        }
    }

    // Otherwise, infer from the body
    let inference_result = infer_body_type(db, value_def);

    let expr_idx = value_def.expression_idx(db);
    inference_result.expression_types[&expr_idx].clone()
}

fn infer_value_signature_cycle_initial(
    _db: &dyn HirInferDatabase,
    _id: salsa::Id,
    _value_def: hir::ValueDef,
) -> InferredType {
    InferredType::Unconstrained
}

/// Convert an AnnotatedType to an InferredType, assigning sequential
/// generic IDs to type variables.
fn annotated_to_inferred(annotated: &AnnotatedType) -> InferredType {
    let mut ctx = ConversionContext::default();
    ctx.convert(annotated)
}

#[derive(Default)]
struct ConversionContext {
    type_var_map: FxHashMap<res::Fql<hir::TypeVariable>, usize>,
    self_type_map: FxHashMap<res::Fql<hir::Trait>, usize>,
    next_id: usize,
}

impl ConversionContext {
    fn fresh_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn type_var_id(&mut self, fql: &res::Fql<hir::TypeVariable>) -> usize {
        if let Some(&id) = self.type_var_map.get(fql) {
            return id;
        }
        let id = self.fresh_id();
        self.type_var_map.insert(fql.clone(), id);
        id
    }

    fn self_type_id(&mut self, trait_fql: &res::Fql<hir::Trait>) -> usize {
        if let Some(&id) = self.self_type_map.get(trait_fql) {
            return id;
        }
        let id = self.fresh_id();
        self.self_type_map.insert(trait_fql.clone(), id);
        id
    }

    fn convert(&mut self, annotated: &AnnotatedType) -> InferredType {
        match annotated {
            AnnotatedType::Missing => InferredType::Missing,
            AnnotatedType::Unconstrained => InferredType::Unconstrained,
            AnnotatedType::Unit => InferredType::Unit,
            AnnotatedType::BuiltIn(builtin) => InferredType::BuiltIn(*builtin),
            AnnotatedType::TypeDef { fql, name, .. } => {
                InferredType::TypeDef(fql.clone(), name.clone())
            }
            AnnotatedType::Lambda { arg, ret } => InferredType::Lambda {
                arg_type: Box::new(self.convert(arg)),
                return_type: Box::new(self.convert(ret)),
            },
            AnnotatedType::Tuple(elements) => {
                let resolved: Vec<InferredType> =
                    elements.iter().map(|e| self.convert(e)).collect();
                InferredType::Tuple(unsafe { NonEmpty::new_unchecked(resolved) })
            }
            AnnotatedType::Bounded { base, args } => InferredType::Bounded {
                base: Box::new(self.convert(base)),
                args: args.iter().map(|a| self.convert(a)).collect(),
            },
            AnnotatedType::TypeVar(AnnotatedTypeVar { fql, name, .. }) => {
                let id = self.type_var_id(fql);
                InferredType::Generic(id, DisplayName::new(name.to_string()))
            }
            AnnotatedType::ConstrainedTypeVar {
                base: AnnotatedTypeVar { fql, name, .. },
                constraints,
                ..
            } => {
                let id = self.type_var_id(fql);
                InferredType::ConstrainedGeneric {
                    id,
                    name: DisplayName::new(name.to_string()),
                    constraints: constraints.clone(),
                }
            }
            AnnotatedType::SelfType {
                trait_fql,
                trait_constraints,
                ..
            } => {
                let id = self.self_type_id(trait_fql);
                let name = DisplayName::new("Self");

                match NonEmpty::try_from(trait_constraints.clone()) {
                    Ok(constraints) => InferredType::ConstrainedGeneric {
                        id,
                        name,
                        constraints,
                    },
                    Err(_) => InferredType::Generic(id, name),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirInferDatabase;

    #[test]
    fn signature_from_monomorphic_annotation() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r#"
            typeof x : Int -> String
            let x = |a| -> ""
            "#,
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        assert_eq!(value_defs.len(), 1);

        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();
        let sig = infer(&db, value_def);
        assert_eq!(
            sig,
            InferredType::Lambda {
                arg_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::Int)),
                return_type: Box::new(InferredType::BuiltIn(hir::BuiltInType::String)),
            }
        );
    }

    #[test]
    fn signature_from_polymorphic_annotation() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            typeof id : t1 -> t1 where
              typevar t1
            let id = |x| -> x
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        assert_eq!(value_defs.len(), 1);

        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        assert_eq!(
            sig,
            InferredType::Lambda {
                arg_type: Box::new(InferredType::Generic(0, DisplayName::new("t1"))),
                return_type: Box::new(InferredType::Generic(0, DisplayName::new("t1"))),
            }
        );
    }

    #[test]
    fn infer_unannotated_literal() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            let x = 1
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        assert_eq!(value_defs.len(), 1);

        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        assert_eq!(sig, InferredType::BuiltIn(hir::BuiltInType::Int));
    }

    #[test]
    fn infer_unannotated_lambda() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            let add = |a, b| -> a + b
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        assert_eq!(value_defs.len(), 1);

        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        // Unannotated lambda: a + b constrains both args to same type
        let type_var = InferredType::Generic(0, DisplayName::new("a0"));
        assert_eq!(
            sig,
            InferredType::Lambda {
                arg_type: Box::new(type_var.clone()),
                return_type: Box::new(InferredType::Lambda {
                    arg_type: Box::new(type_var.clone()),
                    return_type: Box::new(type_var.clone()),
                }),
            }
        );
    }

    /// Bounded type annotation with wrong arity (too many args) gets corrected
    /// by truncating excess args. The corrected annotation is used as the signature.
    #[test]
    fn bounded_annotation_too_many_args_uses_corrected_annotation() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            typedef Box[t] = Box t

            typeof wrong : Box[Int, String]
            let wrong = Box(42)
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        // Annotation Box[Int, String] corrected to Box[Int] (excess args truncated)
        assert_eq!(
            sig,
            InferredType::Bounded {
                base: Box::new(InferredType::TypeDef(
                    res::Fql::new(module_id, alloy_test_harness::idx!(0)),
                    hir::Name::new("Box"),
                )),
                args: vec![InferredType::BuiltIn(hir::BuiltInType::Int)],
            }
        );
    }

    /// Bounded type annotation with wrong arity (too few args) gets corrected
    /// by padding with Unconstrained. The corrected annotation is used as the signature.
    #[test]
    fn bounded_annotation_too_few_args_uses_corrected_annotation() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            typedef Pair[a, b] = Pair a b

            typeof wrong : Pair[Int]
            let wrong = Pair(1, 2)
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        // Annotation Pair[Int] corrected to Pair[Int, _] (missing args padded with Unconstrained)
        assert_eq!(
            sig,
            InferredType::Bounded {
                base: Box::new(InferredType::TypeDef(
                    res::Fql::new(module_id, alloy_test_harness::idx!(0)),
                    hir::Name::new("Pair"),
                )),
                args: vec![
                    InferredType::BuiltIn(hir::BuiltInType::Int),
                    InferredType::Unconstrained,
                ],
            }
        );
    }

    /// Valid bounded type annotation should still work correctly.
    #[test]
    fn bounded_annotation_correct_arity_uses_annotation() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            typedef Box[t] = Box t

            typeof correct : Box[Int]
            let correct = Box(42)
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);
        // Box[Int] has correct arity (1 arg, 1 param) → use annotation
        assert_eq!(
            sig,
            InferredType::Bounded {
                base: Box::new(InferredType::TypeDef(
                    res::Fql::new(module_id, alloy_test_harness::idx!(0)),
                    hir::Name::new("Box"),
                )),
                args: vec![InferredType::BuiltIn(hir::BuiltInType::Int)],
            }
        );
    }

    #[test]
    fn shared_type_vars_get_same_id() {
        let mut db = TestHirInferDatabase::default();
        let module_id = db.add_test_module(
            "test",
            r"
            typeof f : t1 -> t2 -> t1 where
              typevar t1
              typevar t2
            let f = |a, b| -> a
            ",
        );

        let (hir_module, _) = hir::lower_file(&db, module_id);

        let value_defs = hir_module.values().collect::<Vec<_>>();
        assert_eq!(value_defs.len(), 1);

        let value_def = hir::module_value_def(&db, module_id, *value_defs[0].0).unwrap();

        let sig = infer(&db, value_def);

        // t1 -> t2 -> t1: t1 gets id 0, t2 gets id 1, second t1 reuses id 0
        let type_var_t1 = InferredType::Generic(0, DisplayName::new("t1"));
        let type_var_t2 = InferredType::Generic(1, DisplayName::new("t2"));
        assert_eq!(
            sig,
            InferredType::Lambda {
                arg_type: Box::new(type_var_t1.clone()),
                return_type: Box::new(InferredType::Lambda {
                    arg_type: Box::new(type_var_t2.clone()),
                    return_type: Box::new(type_var_t1.clone()),
                }),
            }
        );
    }
}
