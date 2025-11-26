use crate::hir_ty::Fql;
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;

pub fn resolve_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<Fql<hir::Trait>> {
    match path {
        hir::Path::ThisModule(this_path) => {
            get_trait_by_name(db, current_module_id, this_path.first(), scope)
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            get_trait_by_name(db, other_module_id, fqn.module.first(), Scopes::ROOT)
        }
        hir::Path::Unknown(_) => None,
    }

    // get_type_reference_by_name
}

fn get_trait_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::Trait>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((idx, _)) = hir_module.get_trait_by_name(name, scope) else {
        return None;
    };

    Some(Fql::new(module_id, idx))
}
