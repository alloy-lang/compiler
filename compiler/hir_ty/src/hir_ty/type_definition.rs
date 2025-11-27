use crate::hir_ty::ResolvedType;
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;

pub fn type_definition_to_resolved(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> ResolvedType {
    if let Some((resolved_module_id, type_idx)) =
        resolve_type_definition_path(db, current_module_id, path, scope)
    {
        return resolve_type_definition(db, resolved_module_id, type_idx, scope);
    };

    ResolvedType::Unknown
}

fn resolve_type_definition(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeDefinitionIdx,
    _scope: ScopeIdx,
) -> ResolvedType {
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let hir::TypeDefinition { name: _, kind } = hir_module.get_type_definition(type_idx);

    match kind {
        hir::TypeDefinitionKind::Missing => ResolvedType::Unknown,
        hir::TypeDefinitionKind::TypeVariable(_) => todo!("Handle type variables"),
        hir::TypeDefinitionKind::Single(_) => todo!("Handle single type definitions"),
        hir::TypeDefinitionKind::Union(_) => todo!("Handle union type definitions"),
    }
}

fn resolve_type_definition_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<(ModuleId, hir::TypeDefinitionIdx)> {
    match path {
        hir::Path::ThisModule(this_path) => {
            let type_idx =
                get_type_definition_by_name(db, current_module_id, this_path.first(), scope)?;
            Some((current_module_id, type_idx))
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            let type_idx =
                get_type_definition_by_name(db, other_module_id, fqn.module.first(), Scopes::ROOT)?;
            Some((other_module_id, type_idx))
        }
        hir::Path::Unknown(_) => None,
    }
}

fn get_type_definition_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<hir::TypeDefinitionIdx> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((type_idx, _)) = hir_module.get_type_definition_by_name(name, scope) else {
        return None;
    };

    Some(type_idx)
}
