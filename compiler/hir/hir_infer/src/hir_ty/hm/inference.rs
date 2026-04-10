//! Main type inference loop and result conversion

use super::super::Fql;
use super::constraint_gen::infer_expr_hm;
use super::unification::solve_equations;
use super::HMInferenceContext;
use crate::diagnostics::TypeInferenceErrorKind;
use crate::hir_ty::hm::converter::ToInferredTypeConverter;
use crate::{DefinitionInferenceResult, HirInferDatabase, TypeInferenceError};
use alloy_hir_def as hir;
use alloy_hir_resolved::resolve_annotated_type;
use alloy_workspace::ModuleId;

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

    if let Some(type_annotation) = value_def.type_annotation(db) {
        let annotated = resolve_annotated_type(db, module_id, type_annotation);
        let annotated_mono = ctx.converter.annotated_to_mono(&annotated);
        ctx.add_equation(annotated_mono, inferred_mono, expr_fql);
    }

    collect_inference_results(&mut ctx, module_id)
}

fn infer_body_type_cycle_initial(
    _db: &dyn HirInferDatabase,
    _id: salsa::Id,
    _value_def: hir::ValueDef,
) -> DefinitionInferenceResult {
    DefinitionInferenceResult::empty()
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

    collect_inference_results(&mut ctx, module_id)
}

/// Collect inference results from a completed inference context.
///
/// Solves type equations, checks trait constraints, converts MonoTypes
/// to InferredTypes, and collects errors.
fn collect_inference_results(
    ctx: &mut HMInferenceContext<'_>,
    module_id: ModuleId,
) -> DefinitionInferenceResult {
    let db = ctx.db;

    let mut converter = ToInferredTypeConverter::from(&ctx.converter);
    let (substitution, solve_errors) = solve_equations(db, ctx.equations.clone(), &mut converter);

    let mut result = DefinitionInferenceResult::empty();

    // TODO: Instantiate PolyType
    let poly_bodies = ctx.poly_env.iter().map(|(fql, poly)| (fql, &poly.body));
    let mono_bodies = ctx.type_env.iter().map(|(fql, mono)| (fql, mono));

    poly_bodies
        .chain(mono_bodies)
        .filter(|(fql, _)| fql.module_id() == module_id)
        .map(|(fql, mono)| (fql.clone(), substitution.apply(mono)))
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
