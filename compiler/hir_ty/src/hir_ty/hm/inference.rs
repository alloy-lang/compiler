//! Main type inference loop and result conversion

use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

use crate::{HirTyDatabase, HirTypedModule};

use super::super::{check_type_annotation, ExpressionOrPatternIdx, Fql, ResolvedType};
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::{HMInferenceContext, MonoType};

/// Main Hindley-Milner type inference function for a module
///
/// This function performs HM type inference in three phases:
/// 1. Constraint generation: Walk through all expressions and patterns, generating type equations
/// 2. Unification: Solve all type equations to produce a substitution
/// 3. Application: Apply the substitution to all types and convert to ResolvedType
pub fn infer_types_hm(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let mut ctx = HMInferenceContext::new(db);
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Phase 1: Generate constraints for all top-level expressions
    // We iterate through expressions in the order they appear in the module
    for (expression_id, expression, _range, _name_op) in hir_module.expressions() {
        infer_expr_hm(&mut ctx, module_id, expression_id, expression);
    }

    // Phase 1.5: Add type annotation constraints
    // For expressions with type annotations, add equations to unify the inferred type
    // with the annotated type. This allows annotations to guide/constrain inference.
    for (expression_id, _expression, _range, name_op) in hir_module.expressions() {
        if let Some((name, scope)) = name_op {
            let fql = Fql::new(module_id, expression_id);
            let idx = ExpressionOrPatternIdx::Expression(fql);

            // Get the inferred type for this expression
            if let Some(inferred_mono_ty) = ctx.type_env.get(&idx).cloned() {
                // Resolve the type annotation to a ResolvedType
                let annotated_resolved = super::super::type_reference::type_reference_to_resolved(
                    db,
                    module_id,
                    &hir::Path::ThisModule {
                        path: NonEmpty::new(name),
                        scope,
                    },
                    scope,
                );

                // Convert the annotation to MonoType and add unification constraint
                if annotated_resolved != ResolvedType::Unknown {
                    if let Some(annotated_mono) = resolved_to_mono(&annotated_resolved, &mut ctx) {
                        ctx.equations.push(super::TypeEquation {
                            left: inferred_mono_ty,
                            right: annotated_mono,
                            source: idx,
                        });
                    }
                }
            }
        }
    }

    // Phase 2: Solve all accumulated type equations
    match solve_equations(ctx.equations.clone()) {
        Ok(substitution) => {
            // Phase 3: Apply the substitution to all types in the environment
            let mut result = HirTypedModule::empty();

            // Create a shared type variable mapping for the entire module
            // This ensures that the same TypeVarId gets the same Generic ID everywhere
            use super::TypeVarId;
            use rustc_hash::FxHashMap;
            let mut type_var_map: FxHashMap<TypeVarId, usize> = FxHashMap::default();
            let mut next_generic_id = 0;

            for (expression_id, _expression, range, name_op) in hir_module.expressions() {
                let fql = Fql::new(module_id, expression_id);
                let idx = ExpressionOrPatternIdx::Expression(fql);

                if let Some(mono_ty) = ctx.type_env.get(&idx) {
                    let resolved_mono = substitution.apply(mono_ty);
                    let resolved_type = mono_to_resolved_with_map(
                        &resolved_mono,
                        &mut type_var_map,
                        &mut next_generic_id,
                    );
                    result
                        .expression_types
                        .insert(expression_id, resolved_type.clone());

                    // Check for type annotation conflicts
                    check_type_annotation(
                        db,
                        &mut result,
                        module_id,
                        range,
                        name_op,
                        resolved_type,
                    );
                }
            }

            for (pattern_id, _pattern, range, name_op) in hir_module.patterns() {
                let fql = Fql::new(module_id, pattern_id);
                let idx = ExpressionOrPatternIdx::Pattern(fql);

                if let Some(mono_ty) = ctx.type_env.get(&idx) {
                    let resolved_mono = substitution.apply(mono_ty);
                    let resolved_type = mono_to_resolved_with_map(
                        &resolved_mono,
                        &mut type_var_map,
                        &mut next_generic_id,
                    );
                    result
                        .pattern_types
                        .insert(pattern_id, resolved_type.clone());

                    // Check for type annotation conflicts
                    check_type_annotation(
                        db,
                        &mut result,
                        module_id,
                        range,
                        name_op,
                        resolved_type,
                    );
                }
            }

            result
        }
        Err(_unification_error) => {
            // If unification fails, it's likely due to type annotation conflicts
            // Check for conflicting type annotations and report them
            let mut result = HirTypedModule::empty();

            for (expression_id, _expression, range, name_op) in hir_module.expressions() {
                if let Some((name, scope)) = name_op {
                    let fql = Fql::new(module_id, expression_id);
                    let idx = ExpressionOrPatternIdx::Expression(fql);

                    // Get the inferred type for this expression (before unification)
                    if let Some(inferred_mono_ty) = ctx.type_env.get(&idx) {
                        // Resolve the type annotation
                        let expected_type =
                            super::super::type_reference::type_reference_to_resolved(
                                db,
                                module_id,
                                &hir::Path::ThisModule {
                                    path: NonEmpty::new(name),
                                    scope,
                                },
                                scope,
                            );

                        // Skip if no annotation
                        if expected_type == ResolvedType::Unknown {
                            continue;
                        }

                        // Convert the inferred MonoType to ResolvedType (without substitution)
                        let mut dummy_map = rustc_hash::FxHashMap::default();
                        let mut dummy_id = 0;
                        let found_type = mono_to_resolved_with_map(
                            inferred_mono_ty,
                            &mut dummy_map,
                            &mut dummy_id,
                        );

                        // If types don't match, report an error
                        // We do a simple check here since unification already failed
                        if !types_could_unify(&expected_type, &found_type) {
                            result.error(
                                crate::diagnostics::TypeInferenceErrorKind::ConflictingTypeAnnotation {
                                    expected: expected_type,
                                    found: found_type,
                                },
                                range,
                            );
                        }
                    }
                }
            }

            result
        }
    }
}

/// Simple check if two types could potentially unify
/// This is used for error reporting when unification fails
fn types_could_unify(expected: &ResolvedType, found: &ResolvedType) -> bool {
    match (expected, found) {
        // Unknown can unify with anything
        (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => true,
        // Generics can unify with anything
        (ResolvedType::Generic(_), _) | (_, ResolvedType::Generic(_)) => true,
        (ResolvedType::ConstrainedGeneric { .. }, _)
        | (_, ResolvedType::ConstrainedGeneric { .. }) => true,
        // Same types can unify
        (ResolvedType::Unit, ResolvedType::Unit) => true,
        (ResolvedType::BuiltIn(a), ResolvedType::BuiltIn(b)) => a == b,
        (ResolvedType::TypeDef(a), ResolvedType::TypeDef(b)) => a == b,
        // Structural types: check recursively
        (
            ResolvedType::Lambda {
                arg_type: a1,
                return_type: r1,
            },
            ResolvedType::Lambda {
                arg_type: a2,
                return_type: r2,
            },
        ) => types_could_unify(a1, a2) && types_could_unify(r1, r2),
        (ResolvedType::Tuple(e1), ResolvedType::Tuple(e2)) => {
            e1.len() == e2.len()
                && e1
                    .iter()
                    .zip(e2.iter())
                    .all(|(t1, t2)| types_could_unify(t1, t2))
        }
        // Everything else can't unify
        _ => false,
    }
}

/// Convert a ResolvedType to a MonoType for use in constraint generation
/// This allows type annotations to be converted into constraints that guide inference
fn resolved_to_mono(resolved: &ResolvedType, ctx: &mut HMInferenceContext) -> Option<MonoType> {
    match resolved {
        ResolvedType::Unknown => None,
        ResolvedType::Unit => Some(MonoType::Unit),
        ResolvedType::BuiltIn(builtin) => Some(MonoType::Concrete(*builtin)),
        ResolvedType::Lambda {
            arg_type,
            return_type,
        } => {
            let arg_mono = resolved_to_mono(arg_type, ctx)?;
            let ret_mono = resolved_to_mono(return_type, ctx)?;
            Some(MonoType::Function(Box::new(arg_mono), Box::new(ret_mono)))
        }
        ResolvedType::Tuple(elements) => {
            let mono_elements: Option<Vec<_>> =
                elements.iter().map(|e| resolved_to_mono(e, ctx)).collect();
            mono_elements.map(MonoType::Tuple)
        }
        // For Generic types in annotations, create fresh type variables
        // This allows generic annotations to work properly
        ResolvedType::Generic(_) => Some(ctx.fresh_type_var()),
        // For constrained generics, create a fresh type variable
        // TODO: Track the constraints and enforce them during solving
        ResolvedType::ConstrainedGeneric { .. } => Some(ctx.fresh_type_var()),
        // For TypeDef, we don't have a good representation in MonoType yet
        // TODO: Implement proper type definition support
        ResolvedType::TypeDef(_) => None,
        // For Bounded types, not yet implemented
        ResolvedType::Bounded { .. } => None,
    }
}

/// Convert a MonoType to a ResolvedType with a shared type variable mapping
/// This ensures that the same TypeVarId gets the same Generic ID across all
/// expressions and patterns in a module, preserving polymorphic type structure
fn mono_to_resolved_with_map(
    mono: &MonoType,
    type_var_map: &mut rustc_hash::FxHashMap<super::TypeVarId, usize>,
    next_generic_id: &mut usize,
) -> ResolvedType {
    match mono {
        MonoType::Var(var_id) => {
            // Get or assign a canonical ID for this type variable
            let generic_id = *type_var_map.entry(*var_id).or_insert_with(|| {
                let id = *next_generic_id;
                *next_generic_id += 1;
                id
            });
            // TODO: If the type variable has trait constraints in the inference context,
            // create a ConstrainedGeneric instead
            ResolvedType::Generic(generic_id)
        }
        MonoType::Concrete(builtin) => ResolvedType::BuiltIn(*builtin),
        MonoType::Function(arg, ret) => ResolvedType::Lambda {
            arg_type: Box::new(mono_to_resolved_with_map(
                arg,
                type_var_map,
                next_generic_id,
            )),
            return_type: Box::new(mono_to_resolved_with_map(
                ret,
                type_var_map,
                next_generic_id,
            )),
        },
        MonoType::Tuple(elements) => {
            let resolved_elements: Vec<_> = elements
                .iter()
                .map(|e| mono_to_resolved_with_map(e, type_var_map, next_generic_id))
                .collect();
            if resolved_elements.is_empty() {
                ResolvedType::Unknown
            } else {
                let first = resolved_elements[0].clone();
                let rest = resolved_elements.into_iter().skip(1).collect();
                ResolvedType::Tuple(NonEmpty::from((first, rest)))
            }
        }
        MonoType::App {
            constructor: _,
            args: _,
        } => {
            // TODO: Implement proper type application handling
            // For now, return Unknown since this variant isn't used yet
            ResolvedType::Unknown
        }
        MonoType::Unit => ResolvedType::Unit,
    }
}
