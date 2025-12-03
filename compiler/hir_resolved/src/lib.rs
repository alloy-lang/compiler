mod expr;
mod fql;
mod pattern;

use alloy_hir as hir;
pub use expr::*;
pub use fql::*;
pub use pattern::*;

/// Helper function to resolve a cross-module expression reference
fn resolve_cross_module_expression(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::Expression>> {
    let module_slug = fqn.module_slug();
    let other_module_id = db.find_module_by_slug(&*module_slug)?;
    let (hir_module, _) = hir::lower_file(db, other_module_id);
    let (expr_id, _) = hir_module.get_expression_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
    Some(Fql::new(other_module_id, expr_id))
}

/// Helper function to resolve a cross-module pattern reference
fn resolve_cross_module_pattern(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::Pattern>> {
    let module_slug = fqn.module_slug();
    let other_module_id = db.find_module_by_slug(&*module_slug)?;
    let (hir_module, _) = hir::lower_file(db, other_module_id);
    let (pat_id, _) = hir_module.get_pattern_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
    Some(Fql::new(other_module_id, pat_id))
}

/// Helper function to resolve a cross-module type definition reference
fn resolve_cross_module_type_definition(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::TypeDefinition>> {
    let module_slug = fqn.module_slug();
    let other_module_id = db.find_module_by_slug(&*module_slug)?;
    let (hir_module, _) = hir::lower_file(db, other_module_id);
    let (type_def_id, _) =
        hir_module.get_type_definition_by_name(&fqn.name, alloy_scope::Scopes::ROOT)?;
    Some(Fql::new(other_module_id, type_def_id))
}
