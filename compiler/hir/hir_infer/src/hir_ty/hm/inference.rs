//! Main type inference loop and result conversion

use super::super::{Fql, InferredType};
use super::constraint_gen::infer_expr_hm;
use super::unification::{solve_equations, Substitution};
use super::{annotated_to_mono, TypeVarId};
use super::{HMInferenceContext, MonoType};
use crate::diagnostics::TypeInferenceErrorKind;
use crate::{DefinitionInferenceResult, HirInferDatabase, TypeInferenceError};
use alloy_hir_def as hir;
use alloy_hir_resolved::resolve_annotated_type;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

#[salsa::tracked(cycle_initial = infer_body_type_cycle_initial)]
pub(crate) fn infer_body_type<'db>(
    db: &'db dyn HirInferDatabase,
    value_def: hir::ValueDef<'db>,
) -> DefinitionInferenceResult {
    let module_id = value_def.module_id(db);
    let expr_idx = value_def.expression_idx(db);

    let mut ctx = HMInferenceContext::new(db);

    let expr_fql = Fql::new(module_id, expr_idx);
    ctx.inferring_expr = Some(expr_fql.clone());

    let inferred_mono = infer_expr_hm(&mut ctx, expr_fql.clone());

    if let Some(type_annotation) = value_def.type_annotation(db) {
        let annotated = resolve_annotated_type(db, module_id, type_annotation);
        if let Some(annotated_mono) = annotated_to_mono(&annotated, &mut ctx) {
            ctx.add_equation(inferred_mono, annotated_mono, expr_fql);
        }
    }

    let (substitution, unification_errors) = solve_equations(db, ctx.equations.clone());

    collect_inference_results(
        &mut ctx,
        substitution,
        unification_errors,
        module_id,
        Some(Fql::new(module_id, expr_idx)),
    )
}

fn infer_body_type_cycle_initial(
    _db: &dyn HirInferDatabase,
    _id: salsa::Id,
    _value_def: hir::ValueDef,
) -> DefinitionInferenceResult {
    DefinitionInferenceResult::empty()
}

pub(crate) fn infer_expressions(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
) -> DefinitionInferenceResult {
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Handle bare top-level expressions (not bound to a value definition).
    // Use a single shared context so that the `maybe_find_type` cache prevents
    // sub-expressions from being inferred multiple times.
    let mut ctx = HMInferenceContext::new(db);
    for (expr_id, _, _, _) in hir_module.expressions() {
        if hir::module_value_def(db, module_id, expr_id).is_some() {
            continue;
        }
        let expr_fql = Fql::new(module_id, expr_id);
        infer_expr_hm(&mut ctx, expr_fql);
    }
    let (bare_substitution, bare_unification_errors) = solve_equations(db, ctx.equations.clone());

    collect_inference_results(
        &mut ctx,
        bare_substitution,
        bare_unification_errors,
        module_id,
        None,
    )
}

/// Collect inference results from a completed inference context.
///
/// Applies the substitution to all types in the context, converts MonoTypes
/// to InferredTypes, and collects errors.
fn collect_inference_results(
    ctx: &mut HMInferenceContext<'_>,
    substitution: Substitution,
    unification_errors: Vec<TypeInferenceError>,
    module_id: ModuleId,
    definition_fql: Option<Fql<hir::Expression>>,
) -> DefinitionInferenceResult {
    let db = ctx.db;
    let mut type_var_map: FxHashMap<TypeVarId, usize> = FxHashMap::default();
    let mut next_generic_id = 0;

    // Propagate constraints through substitution: if v1 has constraints and
    // v1 → v2 in the substitution, v2 should inherit v1's constraints.
    let constraint_keys: Vec<_> = ctx.constraint_store.keys().copied().collect();
    for var_id in constraint_keys {
        let target_id = substitution.apply_type_var(var_id);
        if target_id != var_id {
            let source_constraints = ctx
                .constraint_store
                .get(&var_id)
                .cloned()
                .unwrap_or_default();
            let store = ctx.constraint_store.entry(target_id).or_default();
            for constraint in source_constraints {
                if !store.contains(&constraint) {
                    store.push(constraint);
                }
            }
        }
    }

    // Get the definition type from type_env
    let definition_type = {
        if let Some(def_fql) = definition_fql {
            if let Some(mono_ty) = ctx.type_env.get(&def_fql.into()) {
                let resolved_ty = substitution.apply(mono_ty);
                mono_to_inferred_with_map(
                    &resolved_ty,
                    &mut type_var_map,
                    &mut next_generic_id,
                    &ctx.resolution_error_vars,
                    &ctx.constraint_store,
                )
            } else {
                InferredType::Unconstrained
            }
        } else {
            InferredType::Unconstrained
        }
    };

    let mut result = DefinitionInferenceResult::empty();

    let poly_bodies = ctx.poly_env.iter().map(|(fql, poly)| (fql, &poly.body));
    let mono_bodies = ctx.type_env.iter().map(|(fql, mono)| (fql, mono));

    poly_bodies
        .chain(mono_bodies)
        .filter(|(fql, _)| fql.module_id() == module_id)
        .map(|(fql, mono)| (fql.clone(), substitution.apply(mono)))
        .map(|(fql, mono)| {
            (
                fql,
                mono_to_inferred_with_map(
                    &mono,
                    &mut type_var_map,
                    &mut next_generic_id,
                    &ctx.resolution_error_vars,
                    &ctx.constraint_store,
                ),
            )
        })
        .for_each(|(fql, resolved)| result.insert_type(fql, resolved));

    let resolution_errors = ctx
        .resolution_errors
        .iter()
        .map(|err| {
            let range = err.get_range(db);
            TypeInferenceError::new(
                TypeInferenceErrorKind::HirResolutionError(err.clone()),
                range,
            )
        })
        .collect::<Vec<_>>();
    result.extend_errors(&resolution_errors);
    result.extend_errors(&unification_errors);

    DefinitionInferenceResult {
        definition_type,
        ..result
    }
}

fn mono_to_inferred_with_map(
    mono: &MonoType,
    type_var_map: &mut FxHashMap<TypeVarId, usize>,
    next_generic_id: &mut usize,
    failed_vars: &FxHashSet<TypeVarId>,
    constraint_store: &FxHashMap<TypeVarId, Vec<(Fql<hir::Trait>, hir::Name)>>,
) -> InferredType {
    match mono {
        MonoType::Unconstrained => InferredType::Unconstrained,
        MonoType::Var(var_id) => {
            if failed_vars.contains(var_id) {
                return InferredType::Missing;
            }
            // Get or assign a canonical ID for this type variable
            let generic_id = *type_var_map.entry(*var_id).or_insert_with(|| {
                let id = *next_generic_id;
                *next_generic_id += 1;
                id
            });
            // If the type variable has trait constraints, create a ConstrainedGeneric
            if let Some(constraints) = constraint_store.get(var_id) {
                if let Some((first, rest)) = constraints.split_first() {
                    return InferredType::ConstrainedGeneric {
                        id: generic_id,
                        constraints: NonEmpty::from((first.clone(), rest.to_vec())),
                    };
                }
            }
            InferredType::Generic(generic_id)
        }
        MonoType::Concrete(builtin) => InferredType::BuiltIn(*builtin),
        MonoType::Function(arg, ret) => InferredType::Lambda {
            arg_type: Box::new(mono_to_inferred_with_map(
                arg,
                type_var_map,
                next_generic_id,
                failed_vars,
                constraint_store,
            )),
            return_type: Box::new(mono_to_inferred_with_map(
                ret,
                type_var_map,
                next_generic_id,
                failed_vars,
                constraint_store,
            )),
        },
        MonoType::Tuple(elements) => {
            let resolved_elements: Vec<_> = elements
                .iter()
                .map(|e| {
                    mono_to_inferred_with_map(
                        e,
                        type_var_map,
                        next_generic_id,
                        failed_vars,
                        constraint_store,
                    )
                })
                .collect();
            if resolved_elements.is_empty() {
                InferredType::Unit
            } else {
                let first = resolved_elements[0].clone();
                let rest = resolved_elements.into_iter().skip(1).collect();
                InferredType::Tuple(NonEmpty::from((first, rest)))
            }
        }
        MonoType::TypeDef {
            fql, type_def_name, ..
        } => InferredType::TypeDef(fql.clone(), type_def_name.clone()),
        MonoType::App { constructor, args } => {
            let base = mono_to_inferred_with_map(
                constructor,
                type_var_map,
                next_generic_id,
                failed_vars,
                constraint_store,
            );
            let resolved_args: Vec<_> = args
                .iter()
                .map(|a| {
                    mono_to_inferred_with_map(
                        a,
                        type_var_map,
                        next_generic_id,
                        failed_vars,
                        constraint_store,
                    )
                })
                .collect();
            InferredType::Bounded {
                base: Box::new(base),
                args: resolved_args,
            }
        }
        MonoType::Unit => InferredType::Unit,
    }
}
