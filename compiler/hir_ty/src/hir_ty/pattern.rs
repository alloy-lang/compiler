use crate::hir_ty::expr::collect_expr_type_fql;
use crate::hir_ty::{
    expr, type_reference, ExpressionOrPatternIdx, Fql, InferenceContext, ResolvedType,
    TypeRequirements,
};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;

pub fn collect_pattern_type_fql(ctx: &mut InferenceContext, fql: &Fql<hir::Pattern>) {
    collect_pattern_type(ctx, fql.module_id, fql.local_id);
}

pub fn collect_pattern_type(
    ctx: &mut InferenceContext,
    current_module_id: ModuleId,
    pattern_id: hir::PatternIdx,
) {
    let pattern = {
        let (hir_module, _) = hir::lower_file(ctx.db, current_module_id);
        hir_module.get_pattern(pattern_id).clone()
    };

    println!("Collecting pattern type: {pattern:?}. id: {pattern_id:?}");
    match &pattern {
        hir::Pattern::Missing => todo!("Missing pattern"),
        hir::Pattern::Literal(lit) => ctx.add_pattern_requirements(
            current_module_id,
            pattern_id,
            TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::from(lit))),
        ),
        hir::Pattern::PatternRef { path, scope } => {
            ctx.insert_pattern_type_variable(current_module_id, pattern_id);

            let type_reference =
                type_reference::type_reference_to_resolved(ctx.db, current_module_id, path, *scope);
            if let ResolvedType::Unknown = type_reference {
            } else {
                ctx.add_pattern_requirements(
                    current_module_id,
                    pattern_id,
                    TypeRequirements::MustBeType(type_reference),
                )
            }

            let other_fql = expr::resolve_path(ctx.db, current_module_id, path, *scope)
                .map(|expr_fql| {
                    collect_expr_type_fql(ctx, &expr_fql);
                    ExpressionOrPatternIdx::Expression(expr_fql)
                })
                .or_else(|| {
                    resolve_path(ctx.db, current_module_id, path, *scope).map(|pattern_fql| {
                        collect_pattern_type_fql(ctx, &pattern_fql);
                        ExpressionOrPatternIdx::Pattern(pattern_fql)
                    })
                });
            if let Some(other_fql) = other_fql {
                ctx.add_pattern_requirements(
                    current_module_id,
                    pattern_id,
                    TypeRequirements::MustBeSameAs(other_fql),
                );
            }
        }
        hir::Pattern::VariableDeclaration { .. } => {
            ctx.insert_pattern_type_variable(current_module_id, pattern_id);
        }
        hir::Pattern::Nil => {
            ctx.add_pattern_requirements(
                current_module_id,
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::Unknown),
            );
        }
        hir::Pattern::Destructure { .. } => {
            // TODO: Handle destructuring patterns
            ctx.insert_pattern_type_variable(current_module_id, pattern_id);
        }
        hir::Pattern::Unit => {
            ctx.add_pattern_requirements(
                current_module_id,
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::Unit),
            );
        }
        hir::Pattern::Tuple(_) => {
            // TODO: Handle tuple patterns
            ctx.insert_pattern_type_variable(current_module_id, pattern_id);
        }
    }
}

pub fn resolve_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<Fql<hir::Pattern>> {
    match path {
        hir::Path::ThisModule(this_path) => {
            get_pattern_by_name(db, current_module_id, this_path.first(), scope)
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            get_pattern_by_name(db, other_module_id, fqn.module.first(), Scopes::ROOT)
        }
        hir::Path::Unknown(_) => None,
    }
}

fn get_pattern_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::Pattern>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((idx, _)) = hir_module.get_pattern_by_name(name, scope) else {
        return None;
    };

    Some(Fql::new(module_id, idx))
}
