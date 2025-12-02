//! Main type inference loop and result conversion

use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;

use super::super::{check_type_annotation, ExpressionOrPatternFql, Fql, ResolvedType};
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::TypeVarId;
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

    for (expression_id, _expression, _range, name_op) in hir_module.expressions() {
        // Phase 1: Generate constraints for all top-level expressions
        infer_expr_hm(&mut ctx, module_id, expression_id);

        // Phase 1.5: Add type annotation constraints
        // For expressions with type annotations, add equations to unify the inferred type
        // with the annotated type. This allows annotations to guide/constrain inference.
        if let Some((name, scope)) = name_op {
            let fql = Fql::new(module_id, expression_id);
            let idx = ExpressionOrPatternFql::Expression(fql);

            // Get the inferred type for this expression
            if let Some(inferred_mono_ty) = ctx.type_env.get(&idx).cloned() {
                // Resolve the type annotation to a ResolvedType
                let Some(annotated_resolved) =
                    super::super::type_reference::type_reference_to_resolved(
                        db,
                        module_id,
                        &hir::Path::ThisModule {
                            path: NonEmpty::new(name),
                            scope,
                        },
                        scope,
                    )
                else {
                    continue;
                };

                // Convert the annotation to MonoType and add unification constraint
                if let Some(annotated_mono) = resolved_to_mono(&annotated_resolved, &mut ctx) {
                    ctx.add_equation(inferred_mono_ty, annotated_mono, idx);
                }
            }
        }
    }

    // Phase 2: Solve all accumulated type equations
    let (substitution, unification_errors) = solve_equations(db, ctx.equations.clone());
    // Phase 3: Apply the substitution to all types in the environment
    let mut result = HirTypedModule::empty();

    // Create a shared type variable mapping for the entire module
    // This ensures that the same TypeVarId gets the same Generic ID everywhere
    let mut type_var_map: FxHashMap<TypeVarId, usize> = FxHashMap::default();
    let mut next_generic_id = 0;

    for (expression_id, _expression, range, name_op) in hir_module.expressions() {
        let fql = Fql::new(module_id, expression_id);
        let idx = ExpressionOrPatternFql::Expression(fql);

        if let Some(mono_ty) = ctx.type_env.get(&idx) {
            let resolved_mono = substitution.apply(mono_ty);
            let resolved_type =
                mono_to_resolved_with_map(&resolved_mono, &mut type_var_map, &mut next_generic_id);
            result
                .expression_types
                .insert(expression_id, resolved_type.clone());

            // Check for type annotation conflicts
            check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
        }
    }

    for (pattern_id, _pattern, range, name_op) in hir_module.patterns() {
        let fql = Fql::new(module_id, pattern_id);
        let idx = ExpressionOrPatternFql::Pattern(fql);

        if let Some(mono_ty) = ctx.type_env.get(&idx) {
            let resolved_mono = substitution.apply(mono_ty);
            let resolved_type =
                mono_to_resolved_with_map(&resolved_mono, &mut type_var_map, &mut next_generic_id);
            result
                .pattern_types
                .insert(pattern_id, resolved_type.clone());

            // Check for type annotation conflicts
            check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
        }
    }

    for err in unification_errors {
        result.push_error(err);
    }

    result
}

/// Convert a ResolvedType to a MonoType for use in constraint generation
/// This allows type annotations to be converted into constraints that guide inference
fn resolved_to_mono(resolved: &ResolvedType, ctx: &mut HMInferenceContext) -> Option<MonoType> {
    match resolved {
        ResolvedType::UnknownReference(_) => None,
        ResolvedType::Unconstrained => Some(MonoType::Unconstrained),
        ResolvedType::Missing => None,
        ResolvedType::TODO => None,
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
        ResolvedType::TypeDef(type_fql) => Some(MonoType::TypeDef(type_fql.clone())),
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
        MonoType::Missing => ResolvedType::Missing,
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
        MonoType::TypeDef(type_fql) => ResolvedType::TypeDef(type_fql.clone()),
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
