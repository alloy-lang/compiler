use crate::Fql;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

pub fn resolve_type_reference_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<Fql<hir::TypeReference>> {
    match path {
        hir::Path::ThisModule {
            name,
            scope: target_scope,
            ..
        } => get_type_reference_by_name(db, current_module_id, name, *target_scope),
        hir::Path::OtherModule(fqn) => resolve_cross_module_type_reference(db, &fqn),
        hir::Path::Unknown(_) => None,
    }
}

fn get_type_reference_by_name(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::TypeReference>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let (type_idx, _) = hir_module.get_type_reference_by_name(name, scope)?;
    Some(Fql::new(module_id, type_idx))
}

/// Helper function to resolve a cross-module expression reference
fn resolve_cross_module_type_reference(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<hir::TypeReference>> {
    let module_slug = fqn.module_slug();
    if fqn.sub_path.is_empty() {
        let Some(other_module_id) = db.find_module_by_slug(&*module_slug) else {
            return None;
        };
        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let Some((expr_id, _)) = hir_module.get_type_reference_by_name(&fqn.name, Scopes::ROOT)
        else {
            return None;
        };
        return Some(Fql::new(other_module_id, expr_id));
    }

    // Build the full path: module + name + sub_path (except last element)
    let full_path: NonEmpty<_> = fqn.segments();

    // Try different splits: start from the end and work backwards
    // For "std::option::Option::Some", try:
    //   1. module="std::option::Option" (probably doesn't exist)
    //   2. module="std::option", type="Option" (this should work!)
    //   3. module="std", type="option" (probably not a type)
    let full_path_length = full_path.len().into();
    for split_point in (1..=full_path_length).rev() {
        let module_path = &full_path[..split_point];
        let type_name = if split_point < full_path_length {
            &full_path[split_point]
        } else {
            continue; // No type name after this split
        };

        // Try to find this module
        let module_slug = module_path
            .iter()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join("::");

        let Some(other_module_id) = db.find_module_by_slug(&module_slug) else {
            continue; // Try next split
        };

        let (hir_module, _) = hir::lower_file(db, other_module_id);

        // Try to find the type in this module
        let Some((expr_id, _)) = hir_module.get_type_reference_by_name(type_name, Scopes::ROOT)
        else {
            // we found the module, but not the type reference
            continue; // Try next split
        };

        return Some(Fql::new(other_module_id, expr_id));
    }

    None
}
