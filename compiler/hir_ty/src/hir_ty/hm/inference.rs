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
            // If unification fails, return an empty result with errors
            // TODO: Convert unification errors to proper type inference errors
            HirTypedModule::empty()
        }
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
            constructor,
            args: _,
        } => {
            // For now, convert to Named type
            ResolvedType::Named(constructor.clone())
        }
        MonoType::Unit => ResolvedType::Unit,
    }
}
