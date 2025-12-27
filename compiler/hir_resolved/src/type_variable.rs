use crate::{resolve_trait_by_ref_id, Fql, TypeResolutionError};
use alloy_hir as hir;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariable {
    Unbound,
    Constrained(NonEmpty<TypeVariableConstraint>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariableConstraint {
    Kind(usize),
    Trait(Fql<hir::Trait>),
}

#[salsa::tracked]
pub fn resolve_type_variable_by_id(
    db: &dyn hir::HirDatabase,
    module_id: alloy_workspace::ModuleId,
    type_def_idx: hir::TypeDefinitionIdx,
) -> (TypeVariable, Vec<TypeResolutionError>) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_def = hir_module.get_type_definition(type_def_idx);

    let tv = match &type_def.kind {
        hir::TypeDefinitionKind::Missing
        | hir::TypeDefinitionKind::Single(_)
        | hir::TypeDefinitionKind::Union(_) => {
            unreachable!("syntax error")
        }
        hir::TypeDefinitionKind::TypeVariable(tv) => tv,
    };

    match tv {
        hir::TypeVariable::Unbound => (TypeVariable::Unbound, vec![]),
        hir::TypeVariable::Constrained(constraints) => {
            let constraint_results = constraints
                .iter()
                .map(|constraint| match constraint {
                    hir::TypeVariableConstraint::Kind(kind) => {
                        Ok(TypeVariableConstraint::Kind(*kind))
                    }
                    hir::TypeVariableConstraint::Trait(trait_path) => {
                        resolve_trait_by_ref_id(db, module_id, *trait_path)
                            .map(|fql_trait| TypeVariableConstraint::Trait(fql_trait))
                    }
                })
                .collect::<Vec<_>>();
            let mut constraints = vec![];
            let mut errors = vec![];
            for result in constraint_results {
                match result {
                    Ok(constraint) => constraints.push(constraint),
                    Err(err) => errors.push(err),
                }
            }

            if constraints.is_empty() {
                (TypeVariable::Unbound, errors)
            } else {
                (
                    TypeVariable::Constrained(unsafe { NonEmpty::new_unchecked(constraints) }),
                    errors,
                )
            }
        }
    }
}
