use crate::hir_ty::{Fql, ResolvedType};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

pub fn type_definition_to_resolved(
    db: &dyn HirTyDatabase,
    ctx: &mut super::type_annotation::TypeResolutionContext,
    current_module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> Option<ResolvedType> {
    let resolved_type_def_fql =
        res::resolve_type_definition_by_ref_id(db, current_module_id, type_idx).ok()?;
    type_definition_to_resolved_type(
        db,
        resolved_type_def_fql.module_id,
        resolved_type_def_fql.local_id,
        ctx,
    )
}

fn type_definition_to_resolved_type(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeDefinitionIdx,
    ctx: &mut super::type_annotation::TypeResolutionContext,
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
                            hir::TypeVariableConstraint::Trait(type_idx) => {
                                alloy_hir_resolved::resolve_trait_by_ref_id(
                                    db,
                                    current_module_id,
                                    *type_idx,
                                )
                                .ok()
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
