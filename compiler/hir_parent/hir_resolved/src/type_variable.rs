use crate::{resolve_trait_by_ref_id, resolver, EPTrFql, Fql, HirResolutionError};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use la_arena::Idx;
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
    type_var_idx: hir::TypeVariableIdx,
) -> (TypeVariable, Vec<HirResolutionError>) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_var = hir_module.get_type_variable(type_var_idx);

    match &type_var.kind {
        hir::TypeVariableKind::Unbound => (TypeVariable::Unbound, vec![]),
        hir::TypeVariableKind::Constrained(constraints) => {
            let constraint_results = constraints
                .iter()
                .map(|constraint| match constraint {
                    hir::TypeVariableConstraint::Kind(kind) => {
                        Ok(TypeVariableConstraint::Kind(*kind))
                    }
                    hir::TypeVariableConstraint::Trait(trait_path) => {
                        resolve_trait_by_ref_id(db, module_id, *trait_path)
                            .map(TypeVariableConstraint::Trait)
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

// ============================================================================
// Type Variable Resolver
// ============================================================================

pub struct TypeVariableResolver;

impl resolver::Resolver<hir::TypeVariable> for TypeVariableResolver {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        scope: ScopeIdx,
    ) -> Option<(Idx<hir::TypeVariable>, hir::TypeVariable)> {
        hir_module
            .get_type_variable_by_name(name, scope)
            .map(|(id, typedef)| (id, typedef.clone()))
    }

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: alloy_workspace::ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> HirResolutionError {
        match source_ref.into() {
            EPTrFql::Expression(fql) => HirResolutionError::UnknownExpressionReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::Pattern(fql) => HirResolutionError::UnknownPatternReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::TypeReference(fql) => HirResolutionError::UnknownTypeReference {
                source_ref: fql,
                module_id,
                path,
            },
        }
    }

    fn validate(
        _db: &dyn hir::HirDatabase,
        _source_ref: impl Into<EPTrFql>,
        _type_def_fql: Fql<hir::TypeVariable>,
        _subname: Option<hir::Name>,
    ) -> Option<HirResolutionError> {
        None
    }
}
