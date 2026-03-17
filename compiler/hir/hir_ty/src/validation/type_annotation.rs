//! Type annotation checking and compatibility validation
//!
//! This module handles checking that inferred types are compatible with their
//! type annotations, including trait constraint verification.

use crate::diagnostics::{ConflictingTypeAnnotationReason, TypeCheckingErrorKind};
use crate::hir_ty::ResolvedType;
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir_def as hir;
use alloy_hir_def::TypeIdx;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, AnnotatedType, Fql};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

pub(crate) fn validate_type_annotations(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    for hir::ValueDefinition {
        type_annotation,
        value,
        ..
    } in hir_module.values().map(|(_, v)| v)
    {
        let Some(resolved_type) = result.expression_types.get(value).cloned() else {
            continue;
        };
        let range = hir_module.get_expression_range(*value);

        if let Some(type_annotation) = type_annotation {
            // let _ = resolve_annotated_expression(db, module_id, *type_annotation, *value);
            // Check for type annotation conflicts
            check_type_annotation(
                db,
                result,
                module_id,
                range,
                *type_annotation,
                resolved_type,
            );
        }
    }
}

fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    type_annotation_idx: TypeIdx,
    resolved_type: ResolvedType,
) {
    let expected_type = resolve_annotated_type(db, current_module_id, type_annotation_idx);

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
                inferred_type: resolved_type,
                reason,
            },
            range,
        );
    }
}

/// Check if the `found` type is compatible with the `expected` annotation type.
/// This compares an AnnotatedType (what the user wrote) against a ResolvedType (what inference produced).
fn check_type_compatibility(
    db: &dyn HirTyDatabase,
    expected: &AnnotatedType,
    found: &ResolvedType,
) -> Result<(), ConflictingTypeAnnotationReason> {
    match (expected, found) {
        // Wildcards on either side
        (AnnotatedType::Unconstrained, _) | (_, ResolvedType::Unconstrained) => Ok(()),
        (AnnotatedType::Missing, _) | (_, ResolvedType::Missing) => {
            Err(ConflictingTypeAnnotationReason::DirectConflict {
                annotated_type: expected.clone(),
                inferred_type: found.clone(),
            })
        }
        (_, ResolvedType::UnknownReference(_)) => Ok(()),

        // Unit
        (AnnotatedType::Unit, ResolvedType::Unit) => Ok(()),

        // Built-in types
        (AnnotatedType::BuiltIn(a), ResolvedType::BuiltIn(b)) if a == b => Ok(()),

        // Nominal type definitions
        (AnnotatedType::TypeDef { fql: a, .. }, ResolvedType::TypeDef(b, _)) if a == b => Ok(()),

        // Type variables in annotation match any generic in inference result
        (AnnotatedType::TypeVar { .. }, ResolvedType::Generic(_)) => Ok(()),
        (AnnotatedType::TypeVar { .. }, ResolvedType::ConstrainedGeneric { .. }) => Ok(()),

        // Constrained type variables - check trait constraints
        (AnnotatedType::ConstrainedTypeVar { constraints, .. }, found_ty) => {
            check_trait_constraints(db, found_ty, constraints)
        }

        // Self type in annotation matches generics
        (AnnotatedType::SelfType { .. }, ResolvedType::Generic(_)) => Ok(()),
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
            ResolvedType::Lambda {
                arg_type,
                return_type,
            },
        ) => {
            check_type_compatibility(db, arg, arg_type)?;
            check_type_compatibility(db, ret, return_type)?;
            Ok(())
        }

        // Tuple types
        (AnnotatedType::Tuple(exp_elems), ResolvedType::Tuple(found_elems)) => {
            if exp_elems.len() != found_elems.len() {
                return Err(ConflictingTypeAnnotationReason::DirectConflict {
                    annotated_type: expected.clone(),
                    inferred_type: found.clone(),
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
            ResolvedType::Bounded {
                base: found_base,
                args: found_args,
            },
        ) => {
            check_type_compatibility(db, exp_base, found_base)?;
            if exp_args.len() != found_args.len() {
                return Err(ConflictingTypeAnnotationReason::DirectConflict {
                    annotated_type: expected.clone(),
                    inferred_type: found.clone(),
                });
            }
            for (exp_arg, found_arg) in exp_args.iter().zip(found_args.iter()) {
                check_type_compatibility(db, exp_arg, found_arg)?;
            }
            Ok(())
        }

        // Everything else is incompatible
        _ => Err(ConflictingTypeAnnotationReason::DirectConflict {
            annotated_type: expected.clone(),
            inferred_type: found.clone(),
        }),
    }
}

/// Check if a type satisfies the given trait constraints
fn check_trait_constraints(
    db: &dyn HirTyDatabase,
    ty: &ResolvedType,
    constraints: &NonEmpty<(Fql<hir::Trait>, hir::Name)>,
) -> Result<(), ConflictingTypeAnnotationReason> {
    match ty {
        ResolvedType::TypeDef(type_fql, _) => {
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
        ResolvedType::Generic(_) | ResolvedType::ConstrainedGeneric { .. } => Ok(()),
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
