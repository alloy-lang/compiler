//! Main type inference loop and result conversion

use super::super::{Fql, InferredType};
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::TypeVarId;
use super::{HMInferenceContext, MonoType};
use crate::diagnostics::TypeInferenceErrorKind;
use crate::{HirInferDatabase, HirInferredModule};
use alloy_hir_def as hir;
use alloy_hir_def::{Name, TypeDefinition};
use alloy_hir_resolved::{resolve_annotated_type, AnnotatedType, EPTdFql, TypeVarReference};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;

/// Main Hindley-Milner type inference function for a module
///
/// This function performs HM type inference with let-polymorphism:
/// 1. Dependency analysis: Build dependency graph and compute topological order
/// 2. For each SCC (strongly-connected component):
///    a. Constraint generation: Generate type equations for all expressions in the SCC
///    b. Unification: Solve all accumulated type equations
///    c. Generalization: For polymorphic type annotations, generalize and store in poly_env
/// 3. Application: Apply final substitution to all types and convert to ResolvedType
pub fn infer_types_hm(db: &dyn HirInferDatabase, module_id: ModuleId) -> HirInferredModule {
    let mut ctx = HMInferenceContext::new(db, module_id);
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Phase 1 & 2: Process expression groups in dependency order
    for (group_idx, group) in hir_module.expression_groups() {
        // Set the current group for lazy constraint generation
        ctx.current_group = Some(group_idx);

        // Capture environment type variables before processing this group
        // These are the type variables from all previous groups
        // IMPORTANT: Skip variables from expressions that have been generalized (in poly_env)
        // because those are now polymorphic and their type variables are quantified
        ctx.env_type_vars.clear();

        for (fql, t) in &ctx.type_env {
            // Skip expressions that have been generalized to poly_env
            if ctx.poly_env.contains_key(fql) {
                continue;
            }

            // Only collect type variables from top-level named expressions (let-bindings)
            // This excludes internal expressions like lambda parameters and bodies
            let should_include = match fql {
                EPTdFql::Expression(expr_fql) => {
                    // Only include if this expression has a name AND is in the same module
                    expr_fql.module_id == module_id
                        && hir_module.get_value_by_id(&expr_fql.local_id).is_some()
                }
                // Patterns are internal to their expressions, don't include them
                EPTdFql::Pattern(_) => false,
                EPTdFql::TypeDefinition(td_fql) | EPTdFql::TypeDefinitionVariant(td_fql, _) => {
                    // Type definitions can be polymorphic (e.g., List[t], Option[t])
                    // Include them if they're in the same module
                    // Unlike expressions, type definitions are always named by definition
                    td_fql.module_id == module_id
                }
            };

            if should_include {
                ctx.env_type_vars.extend(super::free_type_vars(t));
            }
        }

        // 1a. Generate constraints for all expressions in this group
        for &expr_id in group {
            ctx.expr_to_group.insert(expr_id, group_idx);

            let expr_fql = Fql::new(module_id, expr_id);
            infer_expr_hm(&mut ctx, expr_fql.clone());

            // Add type annotation constraints
            // For polymorphic annotations, we skip adding constraints here
            // and instead just use the annotation to guide generalization
            if let Some(value) = hir_module.get_value_by_id(&expr_id) {
                if let Some(type_annotation) = value.type_annotation {
                    // Get the inferred type for this expression
                    if let Some(inferred_mono_ty) = ctx.maybe_find_type(&expr_fql) {
                        let annotated = resolve_annotated_type(db, module_id, type_annotation);

                        // Only add unification constraints for non-polymorphic annotations
                        // Polymorphic annotations are used for generalization instead
                        if !annotated.is_polymorphic() {
                            if let Some(annotated_mono) = annotated_to_mono(&annotated, &mut ctx) {
                                ctx.add_equation(
                                    inferred_mono_ty,
                                    annotated_mono,
                                    expr_fql.clone(),
                                );
                            }
                        }
                    }
                }
            }
        }

        // 1b. Solve all accumulated type equations
        let (substitution, _unification_errors) = solve_equations(db, ctx.equations.clone());

        // 1c. Generalize polymorphic let-bindings
        for &expr_id in group {
            if let Some(value) = hir_module.get_value_by_id(&expr_id) {
                if let Some(type_annotation) = value.type_annotation {
                    let expr_fql = Fql::new(module_id, expr_id);
                    let annotated = resolve_annotated_type(db, module_id, type_annotation);

                    // Get the inferred type and apply substitution
                    if let Some(mono_ty) = ctx.maybe_find_type(&expr_fql) {
                        let resolved_ty = substitution.apply(&mono_ty);

                        // Check if this should be generalized
                        if annotated.is_polymorphic() {
                            // Generalize the type and store in poly_env
                            let poly_ty = ctx.generalize_type(resolved_ty.clone());
                            ctx.poly_env.insert(expr_fql.clone().into(), poly_ty);
                        }

                        // Update type_env with the resolved type
                        ctx.type_env.insert(expr_fql.into(), resolved_ty);
                    }
                }
            }
        }
    }

    // Phase 3: Solve all remaining equations to get final substitution
    let (substitution, unification_errors) = solve_equations(db, ctx.equations.clone());
    // Phase 3: Apply the substitution to all types in the environment
    let mut result = HirInferredModule::empty(module_id);

    // Create a shared type variable mapping for the entire module
    // This ensures that the same TypeVarId gets the same Generic ID everywhere
    let mut type_var_map: FxHashMap<TypeVarId, usize> = FxHashMap::default();
    let mut next_generic_id = 0;

    for (fql, poly_type) in &ctx.poly_env {
        // For polymorphic types, we only need to convert the body
        let mono_ty = &poly_type.body;
        let resolved_type =
            mono_to_resolved_with_map(mono_ty, &mut type_var_map, &mut next_generic_id);

        result.insert_type(fql.clone(), resolved_type.clone());
    }

    for (fql, mono_type) in &ctx.type_env {
        // For monomorphic types, apply substitution first
        let mono_ty = substitution.apply(mono_type);
        let resolved_type =
            mono_to_resolved_with_map(&mono_ty, &mut type_var_map, &mut next_generic_id);

        result.insert_type(fql.clone(), resolved_type.clone());
    }

    // Phase 4: Resolve instantiations to concrete types
    for (def_fql, instantiations) in ctx.instantiations {
        let mut resolved_instantiations = Vec::new();

        for (call_site, fresh_vars) in instantiations {
            // Resolve each fresh variable to a concrete type using the final substitution
            let type_args: Vec<InferredType> = fresh_vars
                .iter()
                .map(|&var_id| {
                    let mono_ty = substitution.apply(&MonoType::Var(var_id));
                    mono_to_resolved_with_map(&mono_ty, &mut type_var_map, &mut next_generic_id)
                })
                .collect();

            resolved_instantiations.push(crate::hir_ty::PolyInstantiation {
                call_site,
                type_args,
            });
        }

        result
            .poly_instantiations
            .insert(def_fql, resolved_instantiations);
    }

    // Convert resolution errors to diagnostics
    for err in ctx.resolution_errors {
        let range = err.get_range(db);
        result.error(TypeInferenceErrorKind::HirResolutionError(err), range);
    }

    for err in unification_errors {
        result.push_error(err);
    }

    result
}

/// Convert an AnnotatedType to a MonoType for use in constraint generation.
/// Uses stable Fql<TypeDefinition> identities for type variables, ensuring
/// the same type variable declaration always maps to the same TypeVarId.
pub(super) fn annotated_to_mono(
    annotated: &AnnotatedType,
    ctx: &mut HMInferenceContext,
) -> Option<MonoType> {
    match annotated {
        AnnotatedType::Missing => None,
        AnnotatedType::Unconstrained => Some(MonoType::Unconstrained),
        AnnotatedType::Unit => Some(MonoType::Unit),
        AnnotatedType::BuiltIn(builtin) => Some(MonoType::Concrete(*builtin)),
        AnnotatedType::TypeDef {
            fql,
            name,
            type_args,
        } => Some(type_def_to_mono(ctx, fql, name, type_args)),
        AnnotatedType::Lambda { arg, ret } => {
            let arg_mono = annotated_to_mono(arg, ctx)?;
            let ret_mono = annotated_to_mono(ret, ctx)?;
            Some(MonoType::Function(Box::new(arg_mono), Box::new(ret_mono)))
        }
        AnnotatedType::Tuple(elements) => {
            let mono_elements: Option<Vec<_>> =
                elements.iter().map(|e| annotated_to_mono(e, ctx)).collect();
            mono_elements.map(MonoType::Tuple)
        }
        AnnotatedType::Bounded { base, args } => {
            let base_mono = annotated_to_mono(base, ctx)?;
            let args_mono: Option<Vec<_>> =
                args.iter().map(|a| annotated_to_mono(a, ctx)).collect();
            Some(MonoType::App {
                constructor: Box::new(base_mono),
                args: args_mono?,
            })
        }
        AnnotatedType::TypeVar { fql, name } => {
            let var_id = ctx.get_or_create_annotation_type_var(fql.clone(), name.clone());
            Some(MonoType::Var(var_id))
        }
        AnnotatedType::ConstrainedTypeVar { fql, name, .. } => {
            // TODO: Track the constraints and enforce them during solving
            let var_id = ctx.get_or_create_annotation_type_var(fql.clone(), name.clone());
            Some(MonoType::Var(var_id))
        }
        AnnotatedType::SelfType { trait_fql, .. } => {
            let var_id = ctx.get_or_create_self_type_var(trait_fql.clone());
            Some(MonoType::Var(var_id))
        }
    }
}

fn type_def_to_mono(
    ctx: &mut HMInferenceContext,
    fql: &Fql<TypeDefinition>,
    name: &Name,
    type_args: &[TypeVarReference],
) -> MonoType {
    let type_args = type_args
        .iter()
        .map(|ty_arg| {
            ctx.get_or_create_annotation_type_var(ty_arg.fql.clone(), ty_arg.name.clone())
        })
        .collect::<Vec<_>>();

    MonoType::TypeDef {
        fql: fql.clone(),
        type_args,
        type_def_name: name.clone(),
    }
}

/// Convert a MonoType to a ResolvedType with a shared type variable mapping
/// This ensures that the same TypeVarId gets the same Generic ID across all
/// expressions and patterns in a module, preserving polymorphic type structure
fn mono_to_resolved_with_map(
    mono: &MonoType,
    type_var_map: &mut FxHashMap<TypeVarId, usize>,
    next_generic_id: &mut usize,
) -> InferredType {
    match mono {
        MonoType::Unconstrained => InferredType::Unconstrained,
        MonoType::Var(var_id) => {
            // Get or assign a canonical ID for this type variable
            let generic_id = *type_var_map.entry(*var_id).or_insert_with(|| {
                let id = *next_generic_id;
                *next_generic_id += 1;
                id
            });
            // TODO: If the type variable has trait constraints in the inference context,
            // create a ConstrainedGeneric instead
            InferredType::Generic(generic_id)
        }
        MonoType::Concrete(builtin) => InferredType::BuiltIn(*builtin),
        MonoType::Function(arg, ret) => InferredType::Lambda {
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
            let base = mono_to_resolved_with_map(constructor, type_var_map, next_generic_id);
            let resolved_args: Vec<_> = args
                .iter()
                .map(|a| mono_to_resolved_with_map(a, type_var_map, next_generic_id))
                .collect();
            InferredType::Bounded {
                base: Box::new(base),
                args: resolved_args,
            }
        }
        MonoType::Unit => InferredType::Unit,
    }
}
