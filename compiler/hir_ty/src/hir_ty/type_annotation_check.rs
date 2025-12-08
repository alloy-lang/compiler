//! Type annotation checking and compatibility validation
//!
//! This module handles checking that inferred types are compatible with their
//! type annotations, including trait constraint verification.

use crate::hir_ty::{Fql, ResolvedType};
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

pub fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    name_op: Option<(hir::Name, ScopeIdx)>,
    resolved_type: ResolvedType,
) {
    // Check for type annotation conflicts
    if let Some((name, scope)) = name_op {
        let Some(expected_type) = super::type_annotation::type_annotation_to_resolved(
            db,
            current_module_id,
            &hir::Path::ThisModule {
                name: name.clone(),
                subname: None,
                scope,
            },
        ) else {
            // Unable to resolve type annotation - skip check
            return;
        };

        // Check if the inferred type is compatible with the expected type
        if let Err(_error) = check_type_compatibility(db, &expected_type, &resolved_type) {
            result.error(
                crate::diagnostics::TypeInferenceErrorKind::ConflictingTypeAnnotation {
                    expected: expected_type,
                    found: resolved_type,
                },
                range,
            );
        }
    }
}

/// Check if the `found` type is compatible with the `expected` type.
/// This is more permissive than equality - it allows:
/// - Generic type variables to match (even with different IDs)
/// - Checking that trait constraints are satisfied
/// - Subtyping relationships (in the future)
fn check_type_compatibility(
    db: &dyn HirTyDatabase,
    expected: &ResolvedType,
    found: &ResolvedType,
) -> Result<(), TypeError> {
    match (expected, found) {
        // Exact matches
        (ResolvedType::UnknownReference(_), _) | (_, ResolvedType::UnknownReference(_)) => Ok(()),
        (ResolvedType::Unconstrained, _) | (_, ResolvedType::Unconstrained) => Ok(()),
        (ResolvedType::Missing, _) | (_, ResolvedType::Missing) => Err(TypeError::Incompatible),
        (ResolvedType::TODO, _) | (_, ResolvedType::TODO) => Err(TypeError::Incompatible),
        (ResolvedType::Unit, ResolvedType::Unit) => Ok(()),
        (ResolvedType::BuiltIn(a), ResolvedType::BuiltIn(b)) if a == b => Ok(()),
        (ResolvedType::TypeDef(a), ResolvedType::TypeDef(b)) if a == b => Ok(()),

        // Generic type variables
        // TODO: Track generic type variable assignments to ensure consistency
        (ResolvedType::Generic(_), ResolvedType::Generic(_)) => Ok(()),

        // Constrained generics - the found type must satisfy the constraints
        (ResolvedType::ConstrainedGeneric { id: _, constraints }, found_ty) => {
            check_trait_constraints(db, found_ty, constraints)
        }

        // A generic can match a constrained generic if we're checking from found -> expected
        // (this allows inference to be more general than the annotation)
        (ResolvedType::Generic(_), ResolvedType::ConstrainedGeneric { .. }) => {
            // TODO: We might want to track that this generic has constraints
            Ok(())
        }

        // Lambda types - check arguments and return types recursively
        (
            ResolvedType::Lambda {
                arg_type: exp_arg,
                return_type: exp_ret,
            },
            ResolvedType::Lambda {
                arg_type: found_arg,
                return_type: found_ret,
            },
        ) => {
            check_type_compatibility(db, exp_arg, found_arg)?;
            check_type_compatibility(db, exp_ret, found_ret)?;
            Ok(())
        }

        // Tuple types - check all elements
        (ResolvedType::Tuple(exp_elems), ResolvedType::Tuple(found_elems)) => {
            if exp_elems.len() != found_elems.len() {
                return Err(TypeError::TupleLengthMismatch);
            }
            for (exp_elem, found_elem) in exp_elems.iter().zip(found_elems.iter()) {
                check_type_compatibility(db, exp_elem, found_elem)?;
            }
            Ok(())
        }

        // Bounded types - check base type and all arguments
        (
            ResolvedType::Bounded {
                base: exp_base,
                args: exp_args,
            },
            ResolvedType::Bounded {
                base: found_base,
                args: found_args,
            },
        ) => {
            // Check that bases are compatible
            check_type_compatibility(db, exp_base, found_base)?;

            // Check argument counts match
            if exp_args.len() != found_args.len() {
                return Err(TypeError::Incompatible);
            }

            // Check all type arguments are compatible
            for (exp_arg, found_arg) in exp_args.iter().zip(found_args.iter()) {
                check_type_compatibility(db, exp_arg, found_arg)?;
            }

            Ok(())
        }

        // Everything else is incompatible
        _ => Err(TypeError::Incompatible),
    }
}

/// Check if a type satisfies the given trait constraints
fn check_trait_constraints(
    db: &dyn HirTyDatabase,
    ty: &ResolvedType,
    constraints: &NonEmpty<Fql<hir::Trait>>,
) -> Result<(), TypeError> {
    match ty {
        // For concrete user-defined types, check if they have behavior implementations
        ResolvedType::TypeDef(type_fql) => {
            // Check each required trait
            for required_trait in constraints.iter() {
                if !has_behavior_for_trait(db, type_fql, required_trait) {
                    return Err(TypeError::ConstraintNotSatisfied);
                }
            }
            Ok(())
        }
        // Generic types can't be checked at compile time
        // They'll be checked when instantiated with concrete types
        ResolvedType::Generic(_) | ResolvedType::ConstrainedGeneric { .. } => Ok(()),
        // For other types (BuiltIn, Lambda, Tuple), we accept them for now
        // TODO: Implement constraint checking for built-in types, lambdas, etc.
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

    // Search through all behaviors in the type's module
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

#[derive(Debug, Clone, PartialEq)]
enum TypeError {
    Incompatible,
    TupleLengthMismatch,
    ConstraintNotSatisfied,
}
