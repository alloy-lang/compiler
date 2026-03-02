//! Main type inference loop and result conversion

use super::super::{check_type_annotation, Fql, ResolvedType};
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::TypeVarId;
use super::{HMInferenceContext, MonoType};
use crate::diagnostics::TypeInferenceErrorKind;
use crate::hir_ty::type_annotation::{type_reference_to_resolved_type, TypeResolutionContext};
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir_resolved::EPTdFql;
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
pub fn infer_types_hm(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let mut ctx = HMInferenceContext::new(db, module_id);
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Phase 0: Build dependency graph and compute topological order
    let dep_graph = super::dependency_analysis::DependencyGraph::build(db, module_id);
    let expression_groups = dep_graph.topological_order();

    // Build a map of expression IDs to their names for efficient lookup
    let expr_names: FxHashMap<hir::ExpressionIdx, hir::ValueDefinition> = hir_module
        .values()
        .map(|value| (value.value, value.clone()))
        .collect();

    // Build expr_to_group mapping for lazy constraint generation
    for (group_idx, group) in expression_groups.iter().enumerate() {
        for &expr_id in group {
            ctx.expr_to_group.insert(expr_id, group_idx);
        }
    }

    // Phase 1 & 2: Process expression groups in dependency order
    for (group_idx, group) in expression_groups.into_iter().enumerate() {
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
                    expr_fql.module_id == module_id && expr_names.contains_key(&expr_fql.local_id)
                }
                // Patterns are internal to their expressions, don't include them
                EPTdFql::Pattern(_) => false,
                EPTdFql::TypeDefinition(td_fql) => {
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
        for &expr_id in &group {
            let expr_fql = Fql::new(module_id, expr_id);
            infer_expr_hm(&mut ctx, expr_fql.clone());

            // Add type annotation constraints
            // For polymorphic annotations, we skip adding constraints here
            // and instead just use the annotation to guide generalization
            if let Some(value) = expr_names.get(&expr_id) {
                if let Some(type_annotation) = value.type_annotation {
                    // Get the inferred type for this expression
                    if let Some(inferred_mono_ty) = ctx.maybe_find_type(&expr_fql) {
                        // Resolve the type annotation to a ResolvedType
                        let mut type_res_ctx = TypeResolutionContext::new();
                        let Some(annotated_resolved) = type_reference_to_resolved_type(
                            db,
                            module_id,
                            type_annotation,
                            &mut type_res_ctx,
                        ) else {
                            continue;
                        };

                        // Only add unification constraints for non-polymorphic annotations
                        // Polymorphic annotations are used for generalization instead
                        if !annotated_resolved.is_polymorphic() {
                            if let Some(annotated_mono) =
                                resolved_to_mono(&annotated_resolved, &mut ctx)
                            {
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
        for &expr_id in &group {
            if let Some(value) = expr_names.get(&expr_id) {
                if let Some(type_annotation) = value.type_annotation {
                    let expr_fql = Fql::new(module_id, expr_id);

                    // Check if this expression has a type annotation
                    let mut type_res_ctx = TypeResolutionContext::new();
                    let annotation_opt = type_reference_to_resolved_type(
                        db,
                        module_id,
                        type_annotation,
                        &mut type_res_ctx,
                    );

                    // Get the inferred type and apply substitution
                    if let Some(mono_ty) = ctx.maybe_find_type(&expr_fql) {
                        let resolved_ty = substitution.apply(&mono_ty);

                        // Check if this should be generalized
                        let should_generalize = annotation_opt
                            .as_ref()
                            .is_some_and(ResolvedType::is_polymorphic);

                        if should_generalize {
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
    let mut result = HirTypedModule::empty();

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

    for hir::ValueDefinition {
        type_annotation,
        value,
        ..
    } in hir_module.values()
    {
        let fql = Fql::new(module_id, *value);
        let Some(resolved_type) = result.expression_types.get(&fql.local_id).cloned() else {
            continue;
        };
        let range = hir_module.get_expression_range(*value);

        if let Some(type_annotation) = type_annotation {
            // Check for type annotation conflicts
            check_type_annotation(
                db,
                &mut result,
                module_id,
                range,
                *type_annotation,
                resolved_type,
            );
        }
    }

    // Phase 4: Resolve instantiations to concrete types
    for (def_fql, instantiations) in ctx.instantiations {
        let mut resolved_instantiations = Vec::new();

        for (call_site, fresh_vars) in instantiations {
            // Resolve each fresh variable to a concrete type using the final substitution
            let type_args: Vec<ResolvedType> = fresh_vars
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
        result.error(TypeInferenceErrorKind::TypeResolutionError(err), range);
    }

    for err in unification_errors {
        result.push_error(err);
    }

    result
}

/// Convert a ResolvedType to a MonoType for use in constraint generation
/// This allows type annotations to be converted into constraints that guide inference
pub(super) fn resolved_to_mono(
    resolved: &ResolvedType,
    ctx: &mut HMInferenceContext,
) -> Option<MonoType> {
    match resolved {
        ResolvedType::UnknownReference(_) => None,
        ResolvedType::Unconstrained => Some(MonoType::Unconstrained),
        ResolvedType::Missing => None,
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
        // Convert TypeDef to MonoType::TypeDef
        ResolvedType::TypeDef(type_fql, name) => {
            Some(MonoType::TypeDef(type_fql.clone(), name.clone()))
        }
        // For Bounded types, convert to MonoType::App
        ResolvedType::Bounded { base, args } => {
            let base_mono = resolved_to_mono(base, ctx)?;
            let args_mono: Option<Vec<_>> = args.iter().map(|a| resolved_to_mono(a, ctx)).collect();
            let args_mono = args_mono?;
            Some(MonoType::App {
                constructor: Box::new(base_mono),
                args: args_mono,
            })
        }
    }
}

/// Convert a MonoType to a ResolvedType with a shared type variable mapping
/// This ensures that the same TypeVarId gets the same Generic ID across all
/// expressions and patterns in a module, preserving polymorphic type structure
fn mono_to_resolved_with_map(
    mono: &MonoType,
    type_var_map: &mut FxHashMap<TypeVarId, usize>,
    next_generic_id: &mut usize,
) -> ResolvedType {
    match mono {
        MonoType::Unconstrained => ResolvedType::Unconstrained,
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
                ResolvedType::Unit
            } else {
                let first = resolved_elements[0].clone();
                let rest = resolved_elements.into_iter().skip(1).collect();
                ResolvedType::Tuple(NonEmpty::from((first, rest)))
            }
        }
        MonoType::TypeDef(type_fql, name) => ResolvedType::TypeDef(type_fql.clone(), name.clone()),
        MonoType::App { constructor, args } => {
            let base = mono_to_resolved_with_map(constructor, type_var_map, next_generic_id);
            let resolved_args: Vec<_> = args
                .iter()
                .map(|a| mono_to_resolved_with_map(a, type_var_map, next_generic_id))
                .collect();
            ResolvedType::Bounded {
                base: Box::new(base),
                args: resolved_args,
            }
        }
        MonoType::Unit => ResolvedType::Unit,
    }
}
