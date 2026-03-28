use super::super::build_constructor_type;
use super::{HMInferenceContext, MonoType};
use alloy_hir_def as hir;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{EPTdFql, Fql};

pub(super) fn infer_data_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: &Fql<hir::TypeDefinition>,
) -> MonoType {
    if let Some(tracked_ty) = ctx.maybe_find_type(type_def_fql) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    let Some(type_def) =
        res::resolve_type_definition_by_id(ctx.db, type_def_fql.module_id, type_def_fql.local_id)
    else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let Some(member) = type_def.get_variant(None) else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let constructor_ty = build_constructor_type(ctx, &type_def_fql, &type_def, member);
    let tracked_ty = ctx.generalize_to_poly(constructor_ty, type_def_fql);
    ctx.assign_type(source_fql, tracked_ty)
}

pub(super) fn infer_variant_constructor(
    ctx: &mut HMInferenceContext,
    source_fql: Fql<hir::Expression>,
    type_def_fql: Fql<hir::TypeDefinition>,
    variant_name: hir::Name,
) -> MonoType {
    let variant_fql = EPTdFql::TypeDefinitionVariant(type_def_fql.clone(), variant_name.clone());
    if let Some(tracked_ty) = ctx.maybe_find_type(variant_fql.clone()) {
        return ctx.assign_type(source_fql, tracked_ty);
    }

    let Some(type_def) =
        res::resolve_type_definition_by_id(ctx.db, type_def_fql.module_id, type_def_fql.local_id)
    else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let Some(member) = type_def.get_variant(Some(&variant_name)) else {
        let ty = ctx.fresh_type_var();
        return ctx.assign_type(source_fql, ty);
    };

    let constructor_ty = build_constructor_type(ctx, &type_def_fql, &type_def, &member);
    let tracked_ty = ctx.generalize_to_poly(constructor_ty, variant_fql);
    ctx.assign_type(source_fql, tracked_ty)
}
