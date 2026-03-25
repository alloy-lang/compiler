use super::super::super::annotated_to_mono;
use super::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, EPTdFql, Fql, TypeDefinitionKind};

pub(super) fn infer_data_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: &Fql<hir::TypeDefinition>,
) -> MonoType {
    // Check if we already have this variant constructor type with tracking
    // This enables polymorphic instantiation tracking for union type variants
    let variant_fql = EPTdFql::TypeDefinition(type_def_fql.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type(variant_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    // Variant not found - return fresh type variable
    let ty = ctx.fresh_type_var();
    ctx.assign_type(source_fql, ty)
}

pub(super) fn infer_variant_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: Fql<hir::TypeDefinition>,
    variant_name: hir::Name,
) -> MonoType {
    // Check if we already have this variant constructor type with tracking
    // This enables polymorphic instantiation tracking for union type variants
    let variant_fql = EPTdFql::TypeDefinitionVariant(type_def_fql.clone(), variant_name.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type(variant_fql.clone()) {
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

    let tracked_ty = ctx.generalize_to_poly(constructor_ty, variant_fql);
    ctx.assign_type(source_fql, tracked_ty)
}

pub(super) fn infer_type_definition(
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

            ctx.generalize_to_poly(constructor_ty, &td_fql)
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

fn build_constructor_type(
    ctx: &mut HMInferenceContext,
    type_def_fql: &Fql<hir::TypeDefinition>,
    type_def: &res::TypeDefinition,
    member: &res::TypeDefinitionMember,
) -> MonoType {
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

    let type_name = type_def_fql.type_def_name(ctx.db);
    let type_var_args: Vec<MonoType> = type_args
        .iter()
        .map(|&var_id| MonoType::Var(var_id))
        .collect();

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
