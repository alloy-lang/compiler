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
/// Uses a single shared HM context for all same-module definitions and
/// expressions, so that call-site constraints flow back to definitions.
///
/// Cross-module references and same-module definitions with polymorphic type
/// annotations use `infer_value_signature` (a Salsa tracked query) for
/// caching and proper instantiation.
///
/// 1. Generate constraints for all expressions in a shared context
/// 2. Solve all type equations
/// 3. Generalize polymorphic-annotated definitions
/// 4. Apply substitution and convert to the module result
pub fn infer_types_hm(db: &dyn HirInferDatabase, module_id: ModuleId) -> HirInferredModule {
    let mut ctx = HMInferenceContext::new(db, module_id);
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Phase 1: Generate constraints for all top-level definitions and bare expressions.
    // Value definitions are inferred via their body expression; sub-expressions
    // are recursively handled by the constraint generation.
    // Set inferring_expr so that infer_expr_hm resolves the body directly
    // instead of going through infer_value_signature (which would lose
    // sub-expression types and error reporting).
    for (&expr_id, value) in hir_module.values() {
        ctx.inferring_expr = Some(Fql::new(module_id, expr_id));
        infer_definition_constraints(&mut ctx, db, module_id, expr_id);
        ctx.inferring_expr = None;

        // For definitions with polymorphic annotations, store in poly_env.
        // This ensures that subsequent references in the shared context get fresh instantiations, preserving let-polymorphism.
        if let Some(type_annotation) = value.type_annotation {
            let annotated = resolve_annotated_type(db, module_id, type_annotation);
            if annotated.is_polymorphic() {
                if let Some(anno_mono) = annotated_to_mono(&annotated, &mut ctx) {
                    let expr_fql = Fql::new(module_id, expr_id);
                    let poly_ty = super::PolyType::generalize_all(anno_mono);
                    ctx.poly_env.insert(expr_fql.into(), poly_ty);
                }
            }
        }
    }

    // Also infer bare top-level expressions (not associated with a value definition)
    for (expr_id, _expr, _range, _name_scope) in hir_module.expressions() {
        if hir_module.get_value_by_id(&expr_id).is_some() {
            continue;
        }
        let expr_fql = Fql::new(module_id, expr_id);
        infer_expr_hm(&mut ctx, expr_fql);
    }

    // Phase 2: Solve all type equations
    let (substitution, unification_errors) = solve_equations(db, ctx.equations.clone());

    // Phase 3: Generalize polymorphic-annotated definitions
    // Must read from type_env directly (not maybe_find_type) because poly_env
    // may contain pre-solving generalizations from Phase 1 — instantiating those
    // would produce fresh variables not present in the substitution.
    for (&expr_id, _value) in hir_module.values() {
        let expr_fql = Fql::new(module_id, expr_id);
        let fql_key: EPTdFql = expr_fql.clone().into();

        if let Some(mono_ty) = ctx.type_env.get(&fql_key).cloned() {
            let resolved_ty = substitution.apply(&mono_ty);
            ctx.generalize_to_poly(resolved_ty, fql_key);
        }
    }

    // Phase 4: Apply substitution and collect results
    let mut result = HirInferredModule::empty(module_id);

    let mut type_var_map: FxHashMap<TypeVarId, usize> = FxHashMap::default();
    let mut next_generic_id = 0;

    for (fql, poly_type) in &ctx.poly_env {
        let mono_ty = substitution.apply(&poly_type.body);
        let resolved_type =
            mono_to_resolved_with_map(&mono_ty, &mut type_var_map, &mut next_generic_id);
        result.insert_type(fql.clone(), resolved_type);
    }

    for (fql, mono_type) in &ctx.type_env {
        let mono_ty = substitution.apply(mono_type);
        let resolved_type =
            mono_to_resolved_with_map(&mono_ty, &mut type_var_map, &mut next_generic_id);
        result.insert_type(fql.clone(), resolved_type);
    }

    for err in ctx.resolution_errors {
        let range = err.get_range(db);
        result.error(TypeInferenceErrorKind::HirResolutionError(err), range);
    }

    for err in unification_errors {
        result.push_error(err);
    }

    result
}

/// Run constraint generation for a single value definition.
///
/// Generates constraints for the body expression and adds annotation
/// constraints if a non-polymorphic type annotation is present.
fn infer_definition_constraints(
    ctx: &mut HMInferenceContext,
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) {
    let expr_fql = Fql::new(module_id, expr_id);
    infer_expr_hm(ctx, expr_fql.clone());

    if let Some(value_def) = hir::module_value_def(db, module_id, expr_id) {
        if let Some(type_annotation) = value_def.type_annotation(db) {
            if let Some(inferred_mono_ty) = ctx.maybe_find_type(&expr_fql) {
                let annotated = resolve_annotated_type(db, module_id, type_annotation);
                if !annotated.is_polymorphic() {
                    if let Some(annotated_mono) = annotated_to_mono(&annotated, ctx) {
                        ctx.add_equation(inferred_mono_ty, annotated_mono, expr_fql);
                    }
                }
            }
        }
    }
}

/// Infer the type of a single definition's body in isolation.
///
/// Creates a fresh HM inference context, runs constraint generation on the body,
/// solves equations, and returns the inferred type.
///
/// This is a Salsa tracked query keyed on ValueDef. Thanks to field-level tracking,
/// it only re-runs when `expression_idx` changes (not when `name` or `type_annotation` change).
#[salsa::tracked(cycle_initial = infer_body_type_cycle_initial)]
pub(super) fn infer_body_type<'db>(
    db: &'db dyn HirInferDatabase,
    value_def: hir::ValueDef<'db>,
) -> InferredType {
    let module_id = value_def.module_id(db);
    let expr_idx = value_def.expression_idx(db);

    let mut ctx = HMInferenceContext::new(db, module_id);

    // Mark this expression as the one being inferred to prevent cycles:
    // infer_expr_hm will skip the infer_value_signature shortcut for this
    // expression and instead resolve it directly from its body.
    ctx.inferring_expr = Some(Fql::new(module_id, expr_idx));

    infer_definition_constraints(&mut ctx, db, module_id, expr_idx);

    let (substitution, _) = solve_equations(db, ctx.equations.clone());

    let fql_key: EPTdFql = Fql::new(module_id, expr_idx).into();
    if let Some(mono_ty) = ctx.type_env.get(&fql_key) {
        let resolved_ty = substitution.apply(mono_ty);
        let mut type_var_map = FxHashMap::default();
        let mut next_id = 0;
        mono_to_resolved_with_map(&resolved_ty, &mut type_var_map, &mut next_id)
    } else {
        InferredType::Unconstrained
    }
}

fn infer_body_type_cycle_initial(
    _db: &dyn HirInferDatabase,
    _id: salsa::Id,
    _value_def: hir::ValueDef,
) -> InferredType {
    InferredType::Unconstrained
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
