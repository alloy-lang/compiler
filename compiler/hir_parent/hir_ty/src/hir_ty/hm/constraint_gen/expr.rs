use super::super::inference::annotated_to_mono;
use super::super::PolyType;
use super::{HMInferenceContext, MonoType};
use crate::hir_ty::hm::constraint_gen::pattern::infer_pattern_hm;
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, EPFql, EPTdFql, Fql, TypeDefinitionKind};
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
    // don't infer it now - just return a fresh type variable.
    // Only applies to expressions in the current module — cross-module expressions
    // have their own independent ordering and must always be inferred immediately.
    // DON'T assign to type_env to avoid polluting env_type_vars for generalization
    if source_fql.module_id == ctx.module_id && ctx.is_in_later_group(source_fql.local_id) {
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
            return ctx.unknown_reference(err, source_fql);
        }
    };

    match expr {
        res::Expression::Literal(lit) => super::infer_literal(ctx, source_fql, lit),
        res::Expression::Unit => super::infer_unit(ctx, source_fql),
        res::Expression::VariableRef(ref_fql) => infer_variable_ref(ctx, source_fql, &ref_fql),
        res::Expression::Lambda { args, body } => infer_lambda(ctx, source_fql, args, body),
        res::Expression::FunctionCall { target, args } => {
            infer_function_call(ctx, source_fql, target, args)
        }
        res::Expression::AbstractTraitFunctionCall {
            type_annotation,
            args,
            ..
        } => infer_abstract_trait_function_call(ctx, source_fql, type_annotation, args),
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
        res::Expression::DataConstructor { type_def } => {
            // Infer the type of the variant constructor
            infer_data_constructor(ctx, source_fql, type_def)
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
    ref_fql: &EPFql,
) -> MonoType {
    // First check if this variable already has a type (possibly polymorphic)
    // This enables let-polymorphism: if the variable has been generalized,
    // we'll instantiate it with fresh type variables
    // Use tracked version: source_fql is the call site, ref_fql is the definition
    if let Some(existing_ty) = ctx.maybe_find_type_tracked(ref_fql.clone(), source_fql.clone()) {
        return ctx.assign_type(source_fql, existing_ty);
    }

    // If not found, infer it (this handles forward references)
    let ty = match ref_fql {
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
                EPTdFql::TypeDefinitionVariant(td_fql, variant_name) => {
                    // Infer the specific variant constructor
                    // This handles tracking if it's polymorphic and returns the instantiated type
                    infer_variant_constructor(
                        ctx,
                        source_fql.clone(),
                        td_fql.clone(),
                        variant_name.clone(),
                    )
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

fn infer_abstract_trait_function_call(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_annotation: Fql<hir::TypeReference>,
    args: Vec<Fql<hir::Expression>>,
) -> MonoType {
    let func_ty = infer_abstract_trait_member_ref(ctx, source_fql.clone(), type_annotation);

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
    type_def: &res::TypeDefinition,
    member: &res::TypeDefinitionMember,
) -> MonoType {
    // let annotated_type_def = res::resolve_type_definition_to_annotated(
    //     ctx.db,
    //     type_def_fql.module_id,
    //     type_def_fql.local_id,
    // );
    // annotated_to_mono(&annotated_type_def, ctx).unwrap_or_else(|| ctx.fresh_type_var());

    let (hir_module, _) = hir::lower_file(ctx.db, type_def_fql.module_id);

    let type_args: Vec<_> = type_def
        .type_args
        .iter()
        .map(|ty_arg| {
            ctx.get_or_create_annotation_type_var(
                ty_arg.clone(),
                hir_module.get_type_variable(ty_arg.local_id).name.clone(),
            )
        })
        .collect::<Vec<_>>();

    // Collect type variable args that are free in the param types.
    // These become the type parameters of the constructor.
    let type_name = type_def_fql.type_def_name(ctx.db);
    let type_var_args: Vec<MonoType> = ctx
        .annotation_type_vars
        .values()
        .filter(|var_id| type_args.contains(var_id))
        .map(|&var_id| MonoType::Var(var_id))
        .collect();

    // Build the result type
    let result_type = if type_args.is_empty() {
        MonoType::TypeDef {
            fql: type_def_fql.clone(),
            type_args,
            type_def_name: type_name,
        }
    } else {
        MonoType::App {
            constructor: Box::new(MonoType::TypeDef {
                fql: type_def_fql.clone(),
                type_args,
                type_def_name: type_name,
            }),
            args: type_var_args,
        }
    };

    // Build curried function type: param1 -> (param2 -> (... -> result))
    member
        .properties()
        .iter()
        .map(|type_idx| {
            let annotated = resolve_annotated_type(ctx.db, type_idx.module_id, type_idx.local_id);
            annotated_to_mono(&annotated, ctx).unwrap_or_else(|| ctx.fresh_type_var())
        })
        .rev()
        .fold(result_type, |acc, param_ty| {
            MonoType::Function(Box::new(param_ty), Box::new(acc))
        })
}

fn infer_data_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: Fql<hir::TypeDefinition>,
) -> MonoType {
    // Check if we already have this variant constructor type with tracking
    // This enables polymorphic instantiation tracking for union type variants
    let variant_fql = EPTdFql::TypeDefinition(type_def_fql.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type_tracked(variant_fql.clone(), source_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    // Variant not found - return fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}

fn infer_variant_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: Fql<hir::TypeDefinition>,
    variant_name: hir::Name,
) -> MonoType {
    // Check if we already have this variant constructor type with tracking
    // This enables polymorphic instantiation tracking for union type variants
    let variant_fql = EPTdFql::TypeDefinitionVariant(type_def_fql.clone(), variant_name.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type_tracked(variant_fql.clone(), source_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    // Find the variant member
    let fdsa = {
        let type_def = res::resolve_type_definition_by_id(
            ctx.db,
            type_def_fql.module_id,
            type_def_fql.local_id,
        );

        match type_def {
            Some(type_def) => match type_def.kind.clone() {
                TypeDefinitionKind::Single(member) => {
                    if member.name() == &variant_name {
                        Some((type_def, member))
                    } else {
                        None
                    }
                }
                TypeDefinitionKind::Union(members) => members
                    .iter()
                    .find(|m| m.name() == &variant_name)
                    .map(|member| (type_def, member.clone())),
            },
            None => None,
        }
    };

    let Some((type_def, member)) = fdsa else {
        // Variant not found - return fresh type variable
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    // Build the constructor type using the shared helper
    let constructor_ty = build_constructor_type(ctx, &type_def_fql, &type_def, &member);

    // Check if this is a polymorphic constructor
    let is_polymorphic = has_type_variables(&constructor_ty);

    if is_polymorphic {
        // Generalize and store in poly_env for proper instantiation
        // For variant constructors, quantify over ALL free variables (not filtered by env_type_vars)
        // since constructors are top-level polymorphic values
        let poly_ty = PolyType::generalize_all(constructor_ty.clone());
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
    let annotated =
        resolve_annotated_type(ctx.db, type_annotation.module_id, type_annotation.local_id);

    if let Some(mono_ty) = annotated_to_mono(&annotated, ctx) {
        return ctx.assign_type(source_fql, mono_ty);
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
    let Some(type_def) =
        res::resolve_type_definition_by_id(ctx.db, td_fql.module_id, td_fql.local_id)
    else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(td_fql, ty);
    };

    let (hir_module, _) = hir::lower_file(ctx.db, td_fql.module_id);

    let ty = match &type_def.kind {
        TypeDefinitionKind::Single(member) => {
            // Single-variant type - treat as a variant constructor
            // For example: typedef Identity[t] = Id t
            // When you call Identity(...), it's the same as Id(...)
            let constructor_ty = build_constructor_type(ctx, &td_fql, &type_def, member);

            // Check if this is a polymorphic constructor (has App with type variables)
            let is_polymorphic = has_type_variables(&constructor_ty);

            if is_polymorphic {
                // Generalize and store in poly_env for proper instantiation
                // For type definition constructors, quantify over ALL free variables
                // since constructors are top-level polymorphic values
                let poly_ty = PolyType::generalize_all(constructor_ty.clone());
                ctx.poly_env.insert(td_fql.clone().into(), poly_ty);
            }

            constructor_ty
        }
        TypeDefinitionKind::Union(_members) => {
            // Multi-variant type - cannot be called as a function directly
            // You must use the specific variant constructor (e.g., Some, None)
            // Return the TypeDef, which will cause a unification error if used as a function
            MonoType::TypeDef {
                fql: td_fql.clone(),
                type_args: type_def
                    .type_args
                    .iter()
                    .map(|ty_arg| {
                        ctx.get_or_create_annotation_type_var(
                            ty_arg.clone(),
                            hir_module.get_type_variable(ty_arg.local_id).name.clone(),
                        )
                    })
                    .collect::<Vec<_>>(),
                type_def_name: type_def.name,
            }
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
        MonoType::Unconstrained
        | MonoType::Concrete(_)
        | MonoType::TypeDef { .. }
        | MonoType::Unit => false,
    }
}
