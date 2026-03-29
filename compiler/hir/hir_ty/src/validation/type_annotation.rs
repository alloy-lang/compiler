//! Type annotation checking and compatibility validation
//!
//! This module handles checking that inferred types are compatible with their
//! type annotations, including trait constraint verification.

use crate::diagnostics::{ConflictingTypeAnnotationReason, TypeCheckingErrorKind};
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir_def as hir;
use alloy_hir_def::TypeIdx;
use alloy_hir_infer::InferredType;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, AnnotatedType, AnnotatedTypeVar, Fql};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

pub(crate) fn validate_type_annotations(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    for value_def in hir_module.values().map(|(_, v)| v) {
        let idx = value_def.expr_idx;
        let Some(resolved_type) = result.expression_types.get(&idx).cloned() else {
            continue;
        };
        let range = hir_module.get_expression_range(idx);

        if let Some(type_annotation) = value_def.type_annotation {
            // Validate arity of bounded types in the annotation
            validate_type_reference_arity(db, result, module_id, type_annotation);
            // Check for type annotation conflicts
            check_type_annotation(db, result, module_id, range, type_annotation, resolved_type);
        }
    }
}

fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    type_annotation_idx: TypeIdx,
    resolved_type: InferredType,
) {
    let expected_type = resolve_annotated_type(db, current_module_id, type_annotation_idx);
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let annotation_range = hir_module.get_type_reference_range(type_annotation_idx);

    // Skip check for missing/unconstrained annotations
    if matches!(
        expected_type,
        AnnotatedType::Missing | AnnotatedType::Unconstrained
    ) {
        return;
    }

    // Check if the inferred type is compatible with the expected type
    if let Err(reason) = check_type_compatibility(db, &expected_type, &resolved_type) {
        result.error(
            TypeCheckingErrorKind::ConflictingTypeAnnotation {
                annotated_type: expected_type,
                annotation_range,
                value_type: resolved_type,
                value_range: range,
                reason,
            },
            range,
        );
    }
}

/// Check if the `found` type is compatible with the `expected` annotation type.
/// This compares an AnnotatedType (what the user wrote) against a InferredType (what inference produced).
fn check_type_compatibility(
    db: &dyn HirTyDatabase,
    expected: &AnnotatedType,
    found: &InferredType,
) -> Result<(), ConflictingTypeAnnotationReason> {
    // TODO: actual ranges for conflicts
    match (expected, found) {
        // Wildcards on either side
        (AnnotatedType::Unconstrained, _) | (_, InferredType::Unconstrained) => Ok(()),
        (AnnotatedType::Missing, _) | (_, InferredType::Missing) => {
            Err(ConflictingTypeAnnotationReason::DirectConflict {
                expected_type: expected.clone(),
                expected_type_range: TextRange::default(),
                actual_type: found.clone(),
                actual_type_range: TextRange::default(),
            })
        }

        // Unit
        (AnnotatedType::Unit, InferredType::Unit) => Ok(()),

        // Built-in types
        (AnnotatedType::BuiltIn(a), InferredType::BuiltIn(b)) if a == b => Ok(()),

        // Nominal type definitions
        (AnnotatedType::TypeDef { fql: a, .. }, InferredType::TypeDef(b, _)) if a == b => Ok(()),

        // Type variables in annotation match any generic in inference result
        (AnnotatedType::TypeVar { .. }, InferredType::Generic(_)) => Ok(()),
        (AnnotatedType::TypeVar { .. }, InferredType::ConstrainedGeneric { .. }) => Ok(()),

        // Constrained type variables - check trait constraints
        (AnnotatedType::ConstrainedTypeVar { constraints, .. }, found_ty) => {
            check_trait_constraints(db, found_ty, constraints)
        }

        // Self type in annotation matches generics
        (AnnotatedType::SelfType { .. }, InferredType::Generic(_)) => Ok(()),
        (
            AnnotatedType::SelfType {
                trait_constraints: constraints,
                ..
            },
            found_ty,
        ) if !constraints.is_empty() => {
            // Self type with constraints — check that the found type satisfies them
            // SAFETY: We just checked non-empty
            let constraints_ne = unsafe { NonEmpty::new_unchecked(constraints.clone()) };
            check_trait_constraints(db, found_ty, &constraints_ne)
        }

        // Lambda types
        (
            AnnotatedType::Lambda { arg, ret },
            InferredType::Lambda {
                arg_type,
                return_type,
            },
        ) => {
            check_type_compatibility(db, arg, arg_type)?;
            check_type_compatibility(db, ret, return_type)?;
            Ok(())
        }

        // Tuple types
        (AnnotatedType::Tuple(exp_elems), InferredType::Tuple(found_elems)) => {
            if exp_elems.len() != found_elems.len() {
                return Err(ConflictingTypeAnnotationReason::DirectConflict {
                    expected_type: expected.clone(),
                    expected_type_range: TextRange::default(),
                    actual_type: found.clone(),
                    actual_type_range: TextRange::default(),
                });
            }
            for (exp_elem, found_elem) in exp_elems.iter().zip(found_elems.iter()) {
                check_type_compatibility(db, exp_elem, found_elem)?;
            }
            Ok(())
        }

        // Bounded types
        (
            AnnotatedType::Bounded {
                base: exp_base,
                args: exp_args,
            },
            InferredType::Bounded {
                base: found_base,
                args: found_args,
            },
        ) => {
            check_type_compatibility(db, exp_base, found_base)?;
            if exp_args.len() != found_args.len() {
                return Err(ConflictingTypeAnnotationReason::DirectConflict {
                    expected_type: expected.clone(),
                    expected_type_range: TextRange::default(),
                    actual_type: found.clone(),
                    actual_type_range: TextRange::default(),
                });
            }
            for (exp_arg, found_arg) in exp_args.iter().zip(found_args.iter()) {
                check_type_compatibility(db, exp_arg, found_arg)?;
            }
            Ok(())
        }

        // Everything else is incompatible
        _ => Err(ConflictingTypeAnnotationReason::DirectConflict {
            expected_type: expected.clone(),
            expected_type_range: TextRange::default(),
            actual_type: found.clone(),
            actual_type_range: TextRange::default(),
        }),
    }
}

/// Check if a type satisfies the given trait constraints
fn check_trait_constraints(
    db: &dyn HirTyDatabase,
    ty: &InferredType,
    constraints: &NonEmpty<(Fql<hir::Trait>, hir::Name)>,
) -> Result<(), ConflictingTypeAnnotationReason> {
    match ty {
        InferredType::TypeDef(type_fql, _) => {
            for (required_trait, _) in constraints {
                if !has_behavior_for_trait(db, type_fql, required_trait) {
                    return Err(
                        ConflictingTypeAnnotationReason::MissingBehaviorImplementation {
                            trait_name: required_trait.trait_name(db),
                            type_name: type_fql.type_def_name(db),
                        },
                    );
                }
            }
            Ok(())
        }
        InferredType::Generic(_) | InferredType::ConstrainedGeneric { .. } => Ok(()),
        _ => Ok(()),
    }
}

/// Check if a type has a behavior implementation for the required trait
fn has_behavior_for_trait(
    db: &dyn HirTyDatabase,
    expected_type_fql: &Fql<hir::TypeDefinition>,
    required_trait: &Fql<hir::Trait>,
) -> bool {
    let (hir_module, _) = hir::lower_file(db, expected_type_fql.module_id);

    for (behavior_idx, _behavior, _range, _name) in hir_module.behaviors() {
        if does_behavior_match(
            db,
            expected_type_fql.module_id,
            behavior_idx,
            expected_type_fql,
            required_trait,
        ) {
            return true;
        }
    }

    false
}

/// Check if a behavior implements the required trait for the given type
fn does_behavior_match(
    db: &dyn HirTyDatabase,
    behavior_module_id: ModuleId,
    behavior_idx: hir::BehaviorIdx,
    expected_type_fql: &Fql<hir::TypeDefinition>,
    required_trait: &Fql<hir::Trait>,
) -> bool {
    let behavior = res::resolve_behavior_by_id(db, behavior_module_id, behavior_idx);
    let Ok(attached_type_fql) = &behavior.attached_type else {
        return false;
    };
    let Ok(attached_trait_fql) = &behavior.attached_trait else {
        return false;
    };

    attached_type_fql != expected_type_fql && required_trait != attached_trait_fql
}

/// Walk a type reference tree and report arity errors at each Bounded node.
fn validate_type_reference_arity(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    module_id: ModuleId,
    type_idx: TypeIdx,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Bounded { base, args } => {
            let base_annotated = resolve_annotated_type(db, module_id, *base);
            let expected_arity = base_annotated.type_arity();

            if expected_arity != args.len() {
                let annotation_range = hir_module.get_type_reference_range(type_idx);
                let type_name = hir::Name::new(format!("{}", base_annotated));
                result.error(
                    TypeCheckingErrorKind::BoundedTypeArityMismatch {
                        type_name,
                        expected_arity,
                        actual_arity: args.len(),
                        annotation_range,
                    },
                    annotation_range,
                );
            }

            // Recurse into args only — base is expected to have arity
            for arg in args {
                validate_type_reference_arity(db, result, module_id, *arg);
            }
        }
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            validate_type_reference_arity(db, result, module_id, *arg_type);
            validate_type_reference_arity(db, result, module_id, *return_type);
        }
        hir::TypeReference::Tuple(types) => {
            for t in types {
                validate_type_reference_arity(db, result, module_id, *t);
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            validate_type_reference_arity(db, result, module_id, *inner);
        }
        // Bare type reference — check if it expects type args
        hir::TypeReference::Named(_) => {
            let resolved = resolve_annotated_type(db, module_id, type_idx);
            let expected_arity = resolved.type_arity();
            if expected_arity > 0 {
                let annotation_range = hir_module.get_type_reference_range(type_idx);
                let type_name = hir::Name::new(format!("{}", resolved));
                result.error(
                    TypeCheckingErrorKind::BoundedTypeArityMismatch {
                        type_name,
                        expected_arity,
                        actual_arity: 0,
                        annotation_range,
                    },
                    annotation_range,
                );
            }
        }
        // Leaf nodes — nothing to check
        hir::TypeReference::Unconstrained
        | hir::TypeReference::Missing
        | hir::TypeReference::SelfRef(_)
        | hir::TypeReference::Unit
        | hir::TypeReference::BuiltIn(_) => {}
    }
}
