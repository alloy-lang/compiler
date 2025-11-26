use crate::hir_ty::ResolvedType;
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;

pub fn type_reference_to_resolved(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> ResolvedType {
    let Some((resolved_module_id, type_idx)) = resolve_path(db, current_module_id, path, scope)
    else {
        return ResolvedType::Unknown;
    };

    resolve_type_reference(db, resolved_module_id, type_idx, scope)
}

fn resolve_type_reference(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeIdx,
    scope: ScopeIdx,
) -> ResolvedType {
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match &type_ref {
        hir::TypeReference::Unconstrained => ResolvedType::Unknown,
        hir::TypeReference::Missing => ResolvedType::Unknown,
        hir::TypeReference::SelfRef => ResolvedType::Unknown, // TODO: Handle self type
        hir::TypeReference::Unit => ResolvedType::Unit,
        hir::TypeReference::Named(path) => {
            type_reference_to_resolved(db, current_module_id, path, scope)
        }
        hir::TypeReference::BuiltIn(built_in) => ResolvedType::BuiltIn(*built_in),
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg = resolve_type_reference(db, current_module_id, *arg_type, scope);
            let ret = resolve_type_reference(db, current_module_id, *return_type, scope);
            ResolvedType::Lambda {
                arg_type: Box::new(arg),
                return_type: Box::new(ret),
            }
        }
        hir::TypeReference::Tuple(types) => {
            if types.is_empty() {
                ResolvedType::Unit
            } else {
                unsafe {
                    let inner_types: Vec<_> = types
                        .iter()
                        .map(|t| resolve_type_reference(db, current_module_id, *t, scope))
                        .collect();
                    ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types))
                }
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            resolve_type_reference(db, current_module_id, *inner, scope)
        }
        hir::TypeReference::Bounded { base: _, args: _ } => {
            todo!("Handle bounded types properly")
        }
    }
}

fn resolve_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<(ModuleId, hir::TypeIdx)> {
    match path {
        hir::Path::ThisModule(this_path) => {
            get_type_reference_by_name(db, current_module_id, this_path.last(), scope)
                .map(|type_idx| (current_module_id, type_idx))
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db
                .find_module_by_slug(&*module_slug)
                .expect("somehow, we couldn't find the module");
            get_type_reference_by_name(db, other_module_id, fqn.module.last(), Scopes::ROOT)
                .map(|type_idx| (other_module_id, type_idx))
        }
        hir::Path::Unknown(_) => None,
    }
}

fn get_type_reference_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<hir::TypeIdx> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((type_idx, _)) = hir_module.get_type_reference_by_name(name, scope) else {
        return None;
    };

    Some(type_idx)
}
