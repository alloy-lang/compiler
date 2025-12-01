use crate::hir_ty::{Fql, ResolvedType};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;

pub fn type_definition_to_resolved(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
    ctx: &mut super::type_reference::TypeResolutionContext,
) -> Option<ResolvedType> {
    let (resolved_module_id, type_idx) =
        resolve_type_definition_path(db, current_module_id, path, scope)?;
    resolve_type_definition(db, resolved_module_id, type_idx, ctx)
}

fn resolve_type_definition(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeDefinitionIdx,
    ctx: &mut super::type_reference::TypeResolutionContext,
) -> Option<ResolvedType> {
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let hir::TypeDefinition { name: _, kind } = hir_module.get_type_definition(type_idx);

    let ty = match kind {
        hir::TypeDefinitionKind::Missing => ResolvedType::Missing,
        hir::TypeDefinitionKind::TypeVariable(type_var) => {
            // Assign a consistent Generic ID for this type variable
            let generic_id = ctx.get_or_assign_id(type_idx);

            match type_var {
                hir::TypeVariable::Unbound => ResolvedType::Generic(generic_id),
                hir::TypeVariable::Constrained(constraints) => {
                    // Convert trait constraints to Fql<hir::Trait> references
                    let trait_constraints: Vec<_> = constraints
                        .iter()
                        .filter_map(|constraint| match constraint {
                            hir::TypeVariableConstraint::Trait(path) => {
                                // Resolve the trait path to get the trait index
                                if let Some((trait_module_id, trait_idx)) =
                                    resolve_trait_path(db, current_module_id, path)
                                {
                                    Some(Fql::new(trait_module_id, trait_idx))
                                } else {
                                    None
                                }
                            }
                            hir::TypeVariableConstraint::Kind(_) => {
                                // Kind constraints aren't trait constraints
                                // todo: kind checking, check against the number of type parameters on the type definition
                                None
                            }
                        })
                        .collect();

                    if trait_constraints.is_empty() {
                        // If no trait constraints (only kind constraints), treat as unconstrained
                        ResolvedType::Generic(generic_id)
                    } else {
                        // SAFETY: We just checked that trait_constraints is non-empty
                        unsafe {
                            let constraints_non_empty = NonEmpty::new_unchecked(trait_constraints);
                            ResolvedType::ConstrainedGeneric {
                                id: generic_id,
                                constraints: constraints_non_empty,
                            }
                        }
                    }
                }
            }
        }
        hir::TypeDefinitionKind::Single(_) | hir::TypeDefinitionKind::Union(_) => {
            // Return a TypeDef pointing to this type definition
            ResolvedType::TypeDef(Fql::new(current_module_id, type_idx))
        }
    };

    Some(ty)
}

// todo: make this return a Fql<hir::Trait>
fn resolve_trait_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
) -> Option<(ModuleId, hir::TraitIdx)> {
    match path {
        hir::Path::ThisModule {
            path: this_path,
            scope: _,
        } => {
            let trait_idx = get_trait_by_name(db, current_module_id, this_path.first())?;
            Some((current_module_id, trait_idx))
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            let trait_idx = get_trait_by_name(db, other_module_id, &fqn.name)?;
            Some((other_module_id, trait_idx))
        }
        hir::Path::Unknown(_) => None,
    }
}

fn get_trait_by_name(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    name: &hir::Name,
) -> Option<hir::TraitIdx> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let Some((trait_idx, _)) = hir_module.get_trait_by_name(name) else {
        return None;
    };

    Some(trait_idx)
}

fn resolve_type_definition_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    _scope: ScopeIdx,
) -> Option<(ModuleId, hir::TypeDefinitionIdx)> {
    match path {
        hir::Path::ThisModule {
            path: this_path,
            scope,
        } => {
            let type_idx =
                get_type_definition_by_name(db, current_module_id, this_path.first(), *scope)?;
            Some((current_module_id, type_idx))
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            let type_idx =
                get_type_definition_by_name(db, other_module_id, &fqn.name, Scopes::ROOT)?;
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
