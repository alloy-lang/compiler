use super::super::inference::resolved_to_mono;
use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use crate::hir_ty::type_annotation::{type_reference_to_resolved_type, TypeResolutionContext};
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{EPFql, EPTdFql, Fql, TypeDefinition, TypeDefinitionKind};
use non_empty_vec::NonEmpty;

/// Generate constraints for an expression using HM inference
pub(crate) fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    // Check if we've already inferred this expression
    if let Some(existing_ty) = ctx.maybe_find_type(&source_fql) {
        return existing_ty;
    }

    // Lazy constraint generation: if this expression is in a later group,
    // don't infer it now - just return a fresh type variable
    // DON'T assign to type_env to avoid polluting env_type_vars for generalization
    if ctx.is_in_later_group(source_fql.local_id) {
        return ctx.fresh_type_var();
    }

    let expr = match alloy_hir_resolved::resolve_expression_by_id(
        ctx.db,
        source_fql.module_id,
        source_fql.local_id,
    ) {
        Ok(expr) => expr,
        Err(err) => {
            // Report the resolution error
            ctx.report_resolution_error(err);
            return ctx.unknown_reference(source_fql);
        }
    };

    match expr {
        res::Expression::Literal(lit) => super::infer_literal(ctx, source_fql, lit),
        res::Expression::Unit => super::infer_unit(ctx, source_fql),
        res::Expression::VariableRef(ref_fql) => infer_variable_ref(ctx, source_fql, ref_fql),
        res::Expression::Lambda { args, body } => infer_lambda(ctx, source_fql, args, body),
        res::Expression::FunctionCall {
            target,
            variant_name,
            args,
        } => infer_function_call(ctx, source_fql, target, variant_name, args),
        res::Expression::Binary { lhs, rhs, .. } => infer_binary(ctx, source_fql, lhs, rhs),
        res::Expression::Tuple(elements) => infer_tuple_expr(ctx, source_fql, elements),
        res::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => infer_if_then_else(ctx, source_fql, condition, then, else_),
        res::Expression::Unary { expression, .. } => infer_unary(ctx, source_fql, expression),
        res::Expression::Match { condition, targets } => {
            infer_match(ctx, source_fql, condition, &targets)
        }
        res::Expression::VariantConstructor {
            type_def,
            variant_name,
        } => {
            // Infer the type of the variant constructor
            infer_variant_constructor(ctx, source_fql, type_def, variant_name)
        }
        res::Expression::AbstractTraitMemberRef {
            trait_fql: _,
            member_name: _,
            type_annotation,
        } => {
            // Abstract trait member references use the type from their type annotation
            infer_abstract_trait_member_ref(ctx, source_fql, type_annotation)
        }
        res::Expression::Missing => infer_missing_expr(ctx, source_fql),
    }
}

fn infer_variable_ref(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    ref_fql: EPFql,
) -> MonoType {
    // First check if this variable already has a type (possibly polymorphic)
    // This enables let-polymorphism: if the variable has been generalized,
    // we'll instantiate it with fresh type variables
    // Use tracked version: source_fql is the call site, ref_fql is the definition
    if let Some(existing_ty) =
        ctx.maybe_find_type_tracked(ref_fql.clone(), source_fql.clone().into())
    {
        return ctx.assign_type(source_fql, existing_ty);
    }

    // If not found, infer it (this handles forward references)
    let ty = match &ref_fql {
        EPFql::Expression(expr_fql) => infer_expr_hm(ctx, expr_fql.clone()),
        EPFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
    };
    ctx.assign_type(source_fql, ty.clone())
}

fn infer_lambda(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    args: Vec<Fql<hir::Pattern>>,
    body: Fql<hir::Expression>,
) -> MonoType {
    // Each lambda parameter gets a fresh type variable
    let arg_types = args
        .iter()
        .map(|arg_id| infer_pattern_hm(ctx, arg_id.clone()))
        .collect::<Vec<_>>();

    // Infer the body type
    let body_ty = infer_expr_hm(ctx, body);

    // Build curried function type: arg1 -> (arg2 -> (... -> body))
    let mut func_ty = body_ty;
    for arg_ty in arg_types.into_iter().rev() {
        func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
    }

    ctx.assign_type(source_fql, func_ty)
}

fn infer_function_call(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    target: EPTdFql,
    variant_name: Option<hir::Name>,
    args: Vec<Fql<hir::Expression>>,
) -> MonoType {
    // Try to get the function type with tracking if it's polymorphic
    // This handles cases like `id("hi")` where the target directly references a polymorphic function
    let func_ty =
        if let Some(tracked_ty) = ctx.maybe_find_type_tracked(target.clone(), source_fql.clone()) {
            // Found a type (possibly polymorphic, now instantiated and tracked)
            tracked_ty
        } else {
            // Not found or not polymorphic yet, infer it
            match &target {
                EPTdFql::Expression(expr_fql) => infer_expr_hm(ctx, expr_fql.clone()),
                EPTdFql::Pattern(pat_fql) => infer_pattern_hm(ctx, pat_fql.clone()),
                EPTdFql::TypeDefinition(td_fql) => {
                    // Check if this is a call to a specific variant (e.g., Option::Some)
                    if let Some(ref vname) = variant_name {
                        // Infer the specific variant constructor
                        // This handles tracking if it's polymorphic and returns the instantiated type
                        infer_variant_constructor(
                            ctx,
                            source_fql.clone(),
                            td_fql.clone(),
                            vname.clone(),
                        )
                    } else {
                        // Calling the type definition itself (single-variant types)
                        // Infer the type definition (e.g., variant constructors)
                        // This will populate poly_env if it's polymorphic
                        infer_type_definition(ctx, td_fql.clone());

                        // Now try to get it again with tracking
                        // If it's polymorphic, it will be in poly_env and we'll track this instantiation
                        if let Some(tracked_ty) =
                            ctx.maybe_find_type_tracked(target.clone(), source_fql.clone())
                        {
                            tracked_ty
                        } else {
                            // Not polymorphic, just get the regular type
                            ctx.maybe_find_type(target.clone())
                                .unwrap_or_else(|| ctx.fresh_type_var())
                        }
                    }
                }
            }
        };

    let arg_types = args
        .iter()
        .map(|arg_fql| infer_expr_hm(ctx, arg_fql.clone()))
        .collect::<Vec<_>>();

    // Build expected function type: arg1 -> (arg2 -> (... -> result))
    let result_ty = ctx.fresh_type_var();
    let mut expected_func_ty = result_ty.clone();
    for arg_ty in arg_types.into_iter().rev() {
        expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
    }

    // Add equation: func_ty = arg1 -> ... -> result
    ctx.add_equation(func_ty, expected_func_ty, source_fql.clone());

    ctx.assign_type(source_fql, result_ty)
}

fn infer_binary(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    lhs: Fql<hir::Expression>,
    rhs: Fql<hir::Expression>,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    // ie. for arithmetic operators, both sides should be numeric types
    // ie. for comparison operators, both sides should be comparable types, etc.
    let lhs_ty = infer_expr_hm(ctx, lhs);
    let rhs_ty = infer_expr_hm(ctx, rhs);

    // For now, assume both sides have the same type and return that type
    ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), source_fql.clone());

    let result_ty = lhs_ty;
    ctx.assign_type(source_fql, result_ty)
}

fn infer_tuple_expr(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    elements: NonEmpty<Fql<hir::Expression>>,
) -> MonoType {
    let element_types = elements
        .iter()
        .map(|elem_id| infer_expr_hm(ctx, elem_id.clone()))
        .collect();

    let ty = MonoType::Tuple(element_types);
    ctx.assign_type(source_fql, ty)
}

fn infer_if_then_else(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    then_branch: Fql<hir::Expression>,
    else_branch: Fql<hir::Expression>,
) -> MonoType {
    // Infer condition type and constrain it to Bool
    let cond_ty = infer_expr_hm(ctx, condition);
    ctx.add_equation(
        cond_ty,
        MonoType::Concrete(hir::BuiltInType::Bool),
        source_fql.clone(),
    );

    let then_ty = infer_expr_hm(ctx, then_branch);
    let else_ty = infer_expr_hm(ctx, else_branch);

    // Both branches must have the same type
    ctx.add_equation(then_ty.clone(), else_ty, source_fql.clone());

    ctx.assign_type(source_fql, then_ty)
}

fn infer_unary(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    inner: Fql<hir::Expression>,
) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    let inner_ty = infer_expr_hm(ctx, inner);
    ctx.assign_type(source_fql, inner_ty)
}

fn infer_match(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    condition: Fql<hir::Expression>,
    targets: &[(Fql<hir::Pattern>, Fql<hir::Expression>)],
) -> MonoType {
    // Infer the scrutinee type
    let value_ty = infer_expr_hm(ctx, condition);

    // Infer all arm patterns and bodies
    let result_ty = ctx.fresh_type_var();

    for (pattern_id, body_id) in targets {
        // Pattern must match the scrutinee type
        let pattern_ty = infer_pattern_hm(ctx, pattern_id.clone());
        ctx.add_equation(value_ty.clone(), pattern_ty, pattern_id.clone());

        // Body must have the same type as other arms
        let body_ty = infer_expr_hm(ctx, body_id.clone());
        ctx.add_equation(result_ty.clone(), body_ty, body_id.clone());
    }

    ctx.assign_type(source_fql, result_ty)
}

pub(super) fn infer_missing_expr(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
) -> MonoType {
    // Missing expressions get a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}

/// Build a constructor type from a variant member
/// Returns: param1 -> param2 -> ... -> TypeDef[type_args]
fn build_constructor_type(
    ctx: &mut HMInferenceContext,
    type_def_fql: &Fql<hir::TypeDefinition>,
    member: &res::TypeDefinitionMember,
) -> MonoType {
    use super::super::TypeVarId;
    use rustc_hash::FxHashMap;

    // Create a shared type resolution context for all parameters
    // This ensures consistent Generic IDs across all type references
    let mut type_ctx = TypeResolutionContext::new();

    // Mapping from Generic ID to MonoType::Var for type parameters
    let mut generic_to_var: FxHashMap<usize, TypeVarId> = FxHashMap::default();

    // Helper to convert ResolvedType to MonoType with consistent type variable mapping
    let resolved_to_mono_tracked = |resolved: &crate::hir_ty::ResolvedType,
                                    ctx: &mut HMInferenceContext,
                                    generic_map: &mut FxHashMap<usize, TypeVarId>|
     -> Option<MonoType> {
        use crate::hir_ty::ResolvedType;
        match resolved {
            ResolvedType::Generic(id) => {
                // Use or create a type variable for this generic ID
                let var_id = *generic_map.entry(*id).or_insert_with(|| {
                    let fresh = ctx.type_var_gen.fresh();
                    fresh
                });
                Some(MonoType::Var(var_id))
            }
            ResolvedType::ConstrainedGeneric { id, .. } => {
                // Treat constrained generics the same for now
                let var_id = *generic_map.entry(*id).or_insert_with(|| {
                    let fresh = ctx.type_var_gen.fresh();
                    fresh
                });
                Some(MonoType::Var(var_id))
            }
            other => resolved_to_mono(other, ctx),
        }
    };

    // Get the parameter types from the variant's properties
    let param_types: Vec<MonoType> = member
        .properties()
        .iter()
        .map(|type_idx| {
            // Convert TypeIdx to MonoType
            if let Some(resolved) = type_reference_to_resolved_type(
                ctx.db,
                type_idx.module_id,
                type_idx.local_id,
                &mut type_ctx,
            ) {
                // Convert ResolvedType to MonoType with tracked generics
                resolved_to_mono_tracked(&resolved, ctx, &mut generic_to_var)
                    .unwrap_or_else(|| ctx.fresh_type_var())
            } else {
                ctx.fresh_type_var()
            }
        })
        .collect();

    // Build the result type
    let result_type = if generic_to_var.is_empty() {
        // No type parameters - just the TypeDef
        MonoType::TypeDef(type_def_fql.clone())
    } else {
        // Has type parameters - build App with type arguments
        // Sort by generic ID to ensure consistent ordering
        let mut type_vars: Vec<_> = generic_to_var.iter().collect();
        type_vars.sort_by_key(|(id, _)| *id);
        let args: Vec<MonoType> = type_vars
            .into_iter()
            .map(|(_, &var_id)| MonoType::Var(var_id))
            .collect();

        MonoType::App {
            constructor: Box::new(MonoType::TypeDef(type_def_fql.clone())),
            args,
        }
    };

    // Build curried function type: param1 -> (param2 -> (... -> result))
    if param_types.is_empty() {
        // No parameters - the variant is just the result type (e.g., None)
        result_type
    } else {
        param_types
            .into_iter()
            .rev()
            .fold(result_type, |acc, param_ty| {
                MonoType::Function(Box::new(param_ty), Box::new(acc))
            })
    }
}

fn infer_variant_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: Fql<hir::TypeDefinition>,
    variant_name: hir::Name,
) -> MonoType {
    // Check if we already have this variant constructor type with tracking
    // This enables polymorphic instantiation tracking for union type variants
    let variant_fql = EPTdFql::TypeDefinition(type_def_fql.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type_tracked(variant_fql.clone(), source_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    // Find the variant member
    let variant_member = {
        let type_def = res::resolve_type_definition_by_id(
            ctx.db,
            type_def_fql.module_id,
            type_def_fql.local_id,
        );

        match type_def {
            Some(TypeDefinition {
                name: _,
                kind: TypeDefinitionKind::Single(member),
            }) => {
                if member.name() == &variant_name {
                    Some(member)
                } else {
                    None
                }
            }
            Some(TypeDefinition {
                name: _,
                kind: TypeDefinitionKind::Union(members),
            }) => members.iter().find(|m| m.name() == &variant_name).cloned(),
            None => None,
        }
    };

    let Some(member) = variant_member else {
        // Variant not found - return fresh type variable
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    // Build the constructor type using the shared helper
    let constructor_ty = build_constructor_type(ctx, &type_def_fql, &member);

    // Check if this is a polymorphic constructor
    let is_polymorphic = has_type_variables(&constructor_ty);

    if is_polymorphic {
        // Generalize and store in poly_env for proper instantiation
        // For variant constructors, quantify over ALL free variables (not filtered by env_type_vars)
        // since constructors are top-level polymorphic values
        use rustc_hash::FxHashSet;
        let poly_ty =
            super::super::PolyType::generalize(constructor_ty.clone(), &FxHashSet::default());
        ctx.poly_env.insert(variant_fql.clone(), poly_ty);

        // Now get it again with tracking to record this instantiation
        if let Some(tracked_ty) = ctx.maybe_find_type_tracked(variant_fql, source_fql.clone()) {
            return ctx.assign_type(source_fql, tracked_ty);
        }
    }

    ctx.assign_type(source_fql, constructor_ty)
}

fn infer_abstract_trait_member_ref(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_annotation: Fql<hir::TypeReference>,
) -> MonoType {
    // Resolve the type annotation to get the type for this abstract member
    // Create a temporary type resolution context
    let mut type_ctx = TypeResolutionContext::new();

    if let Some(resolved_type) = type_reference_to_resolved_type(
        ctx.db,
        type_annotation.module_id,
        type_annotation.local_id,
        &mut type_ctx,
    ) {
        // Convert the resolved type to a monotype
        if let Some(mono_ty) = resolved_to_mono(&resolved_type, ctx) {
            return ctx.assign_type(source_fql, mono_ty);
        }
    }

    // If we can't resolve the type annotation, use a fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}

fn infer_type_definition(
    ctx: &mut HMInferenceContext,
    td_fql: Fql<hir::TypeDefinition>,
) -> MonoType {
    // Check if already inferred - don't use cached polymorphic types
    // Polymorphic types are stored in poly_env and instantiated with fresh variables
    if let Some(existing) = ctx.maybe_find_type(&td_fql) {
        return existing;
    }

    // Resolve the type definition to get its kind
    let type_def = res::resolve_type_definition_by_id(ctx.db, td_fql.module_id, td_fql.local_id);

    let ty = match type_def {
        Some(TypeDefinition {
            name: _,
            kind: TypeDefinitionKind::Single(member),
        }) => {
            // Single-variant type - treat as a variant constructor
            // For example: typedef Identity[t] = Id t
            // When you call Identity(...), it's the same as Id(...)
            let constructor_ty = build_constructor_type(ctx, &td_fql, &member);

            // Check if this is a polymorphic constructor (has App with type variables)
            let is_polymorphic = has_type_variables(&constructor_ty);

            if is_polymorphic {
                // Generalize and store in poly_env for proper instantiation
                // For type definition constructors, quantify over ALL free variables
                // since constructors are top-level polymorphic values
                use rustc_hash::FxHashSet;
                let poly_ty = super::super::PolyType::generalize(
                    constructor_ty.clone(),
                    &FxHashSet::default(),
                );
                ctx.poly_env.insert(td_fql.clone().into(), poly_ty);
            }

            constructor_ty
        }
        Some(TypeDefinition {
            name: _,
            kind: TypeDefinitionKind::Union(_members),
        }) => {
            // Multi-variant type - cannot be called as a function directly
            // You must use the specific variant constructor (e.g., Some, None)
            // Return the TypeDef, which will cause a unification error if used as a function
            MonoType::TypeDef(td_fql.clone())
        }
        None => {
            // Type definition not found or couldn't be resolved
            ctx.fresh_type_var()
        }
    };

    ctx.assign_type(td_fql, ty)
}

/// Check if a MonoType contains type variables (is polymorphic)
fn has_type_variables(ty: &MonoType) -> bool {
    match ty {
        MonoType::Var(_) => true,
        MonoType::Function(arg, ret) => has_type_variables(arg) || has_type_variables(ret),
        MonoType::Tuple(elements) => elements.iter().any(has_type_variables),
        MonoType::App { constructor, args } => {
            has_type_variables(constructor) || args.iter().any(has_type_variables)
        }
        MonoType::Unconstrained | MonoType::Concrete(_) | MonoType::TypeDef(_) | MonoType::Unit => {
            false
        }
    }
}
