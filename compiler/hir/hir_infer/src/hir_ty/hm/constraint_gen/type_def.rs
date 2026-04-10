use crate::hir_ty::hm::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, EPTdFql, Fql};

pub(in crate::hir_ty::hm::constraint_gen) fn infer_data_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: impl Into<EPTdFql>,
    type_def_fql: &Fql<hir::TypeDefinition>,
) -> MonoType {
    let variant_fql = EPTdFql::TypeDefinition(type_def_fql.clone());
    let variant_name = None;

    inner(ctx, source_fql, type_def_fql, variant_name, variant_fql)
}

pub(in crate::hir_ty::hm::constraint_gen) fn infer_variant_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: impl Into<EPTdFql>,
    type_def_fql: &Fql<hir::TypeDefinition>,
    variant_name: hir::Name,
) -> MonoType {
    let variant_fql = EPTdFql::TypeDefinitionVariant(type_def_fql.clone(), variant_name.clone());
    let variant_name = Some(&variant_name);

    inner(ctx, source_fql, type_def_fql, variant_name, variant_fql)
}

fn inner(
    ctx: &mut HMInferenceContext,
    source_fql: impl Into<EPTdFql>,
    type_def_fql: &Fql<hir::TypeDefinition>,
    variant_name: Option<&hir::Name>,
    variant_fql: EPTdFql,
) -> MonoType {
    if let Some(tracked_ty) = ctx.maybe_find_type(variant_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    let Some(type_def) = res::resolve_type_definition_by_fql(ctx.db, type_def_fql) else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let Some(member) = type_def.get_variant(variant_name) else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let constructor_ty = build_constructor_type(ctx, type_def_fql, &type_def, member);
    let tracked_ty = ctx.generalize_to_poly(constructor_ty, variant_fql);
    ctx.assign_type(source_fql, tracked_ty)
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
            ctx.converter.get_or_create_annotation_type_var(
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
            ctx.converter.annotated_to_mono(&annotated)
        })
        .rev()
        .fold(result_type, |acc, param_ty| {
            MonoType::Function(Box::new(param_ty), Box::new(acc))
        })
}
