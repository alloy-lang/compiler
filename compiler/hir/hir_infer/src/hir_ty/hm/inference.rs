//! Main type inference loop and result conversion

use super::super::Fql;
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::HMInferenceContext;
use crate::diagnostics::TypeInferenceErrorKind;
use crate::hir_ty::hm::converter::ToInferredTypeConverter;
use crate::{DefinitionInferenceResult, HirInferDatabase, InferredType, TypeInferenceError};
use alloy_hir_def as hir;
use alloy_hir_resolved::{resolve_annotated_type, EPTdFql};
use alloy_workspace::ModuleId;
use itertools::Itertools;
use rustc_hash::FxHashSet;

#[salsa::tracked(cycle_initial = infer_body_type_cycle_initial)]
pub(crate) fn infer_body_type<'db>(
    db: &'db dyn HirInferDatabase,
    value_def: hir::ValueDef<'db>,
) -> DefinitionInferenceResult {
    let module_id = value_def.module_id(db);
    let expr_idx = value_def.expression_idx(db);

    let mut ctx = HMInferenceContext::new(db);

    let expr_fql = Fql::new(module_id, expr_idx);
    ctx.inferring_expr = Some(expr_fql.clone());

    let inferred_mono = infer_expr_hm(&mut ctx, expr_fql.clone());

    let local_annotation_var_ids = {
        let vars_before = ctx.converter.annotation_var_ids();
        if let Some(type_annotation) = value_def.type_annotation(db) {
            let annotated = resolve_annotated_type(db, module_id, type_annotation);
            let annotated_mono = ctx.converter.annotated_to_mono(&annotated);
            ctx.add_equation(annotated_mono, inferred_mono, expr_fql);
        }
        let vars_after = ctx.converter.annotation_var_ids();
        &vars_after - &vars_before
    };

    collect_inference_results(&mut ctx, module_id, local_annotation_var_ids)
}

fn infer_body_type_cycle_initial<'db>(
    db: &'db dyn HirInferDatabase,
    _id: salsa::Id,
    value_def: hir::ValueDef<'db>,
) -> DefinitionInferenceResult {
    let mut result = DefinitionInferenceResult::empty();
    result.expression_types.insert(
        value_def.expression_idx(db),
        InferredType::Unconstrained,
    );
    result
}

pub(crate) fn infer_expressions(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
) -> DefinitionInferenceResult {
    let (hir_module, _) = hir::lower_file(db, module_id);

    let mut ctx = HMInferenceContext::new(db);
    for (expr_id, _, _, _) in hir_module.expressions() {
        if hir::module_value_def(db, module_id, expr_id).is_some() {
            continue;
        }
        let expr_fql = Fql::new(module_id, expr_id);
        infer_expr_hm(&mut ctx, expr_fql);
    }

    collect_inference_results(&mut ctx, module_id, FxHashSet::default())
}

/// Collect inference results from a completed inference context.
///
/// Solves type equations, checks trait constraints, converts MonoTypes
/// to InferredTypes, and collects errors.
///
/// `local_annotation_var_ids` are the type vars from the definition's own
/// annotation — only these names are propagated to inferred types.
fn collect_inference_results(
    ctx: &mut HMInferenceContext<'_>,
    module_id: ModuleId,
    local_annotation_var_ids: FxHashSet<super::TypeVarId>,
) -> DefinitionInferenceResult {
    let db = ctx.db;

    let mut converter = ToInferredTypeConverter::from(
        &ctx.converter,
        &local_annotation_var_ids.into_iter().collect::<Vec<_>>(),
    );
    let (substitution, solve_errors) = solve_equations(db, ctx.equations.clone(), &mut converter);

    let mut result = DefinitionInferenceResult::empty();

    // TODO: Instantiate PolyType
    let poly_bodies = ctx.poly_env.iter().map(|(fql, poly)| (fql, &poly.body));
    let mono_bodies = ctx.type_env.iter().map(|(fql, mono)| (fql, mono));

    poly_bodies
        .chain(mono_bodies)
        .filter(|(fql, _)| fql.module_id() == module_id)
        .map(|(fql, mono)| (fql.clone(), substitution.apply(&mono)))
        .sorted_by_key(|(fql, _)| lexical_sort_key(fql))
        .map(|(fql, mono)| {
            (
                fql,
                converter.mono_to_inferred(&mono, &ctx.resolution_error_vars),
            )
        })
        .for_each(|(fql, resolved)| result.insert_type(fql, resolved));

    let resolution_errors = ctx
        .resolution_errors
        .iter()
        .map(|err| {
            let range = err.get_range(db);
            TypeInferenceError::new(
                TypeInferenceErrorKind::HirResolutionError(err.clone()),
                range,
            )
        })
        .collect::<Vec<_>>();

    result.extend_errors(&resolution_errors);
    result.extend_errors(&solve_errors);

    result
}

/// intra-module sorting
fn lexical_sort_key(fql: &EPTdFql) -> (u8, u32) {
    match fql {
        EPTdFql::Pattern(fql) => (0, u32::from(fql.local_id.into_raw())),
        EPTdFql::Expression(fql) => (1, u32::from(fql.local_id.into_raw())),
        EPTdFql::TypeDefinition(fql) => (2, u32::from(fql.local_id.into_raw())),
        EPTdFql::TypeDefinitionVariant(fql, _) => (3, u32::from(fql.local_id.into_raw())),
    }
}
