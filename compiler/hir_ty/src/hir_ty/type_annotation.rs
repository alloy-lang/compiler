use crate::hir_ty::{AnnotatedType, Fql};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_hir::{Name, Trait};
use alloy_hir_resolved as res;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use std::convert::TryFrom;

/// Resolve a type annotation to an AnnotatedType.
///
/// Uses stable HIR identities (Fql<TypeDefinition>) for type variables instead
/// of sequential IDs. This is a pure function of its inputs (no mutable context),
/// so it can be Salsa-tracked for incremental computation.
#[salsa::tracked]
pub fn resolve_type_annotation(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Unconstrained => AnnotatedType::Unconstrained,
        hir::TypeReference::Missing => AnnotatedType::Missing,
        hir::TypeReference::Unit => AnnotatedType::Unit,
        hir::TypeReference::BuiltIn(built_in) => AnnotatedType::BuiltIn(*built_in),

        hir::TypeReference::SelfRef(scope) => {
            // In a behavior, Self resolves to the concrete attached_type
            if let Some((_idx, behavior)) = hir_module.find_behavior_containing_scope(*scope) {
                resolve_type_annotation(db, module_id, behavior.attached_type)
            }
            // In a trait, Self is a polymorphic type variable
            else if let Some((trait_idx, trait_def)) =
                hir_module.find_trait_containing_scope(*scope)
            {
                let trait_fql = Fql::new(module_id, trait_idx);
                let trait_constraints: Vec<_> = trait_def
                    .self_constraints()
                    .iter()
                    .filter_map(|constraint| trait_constraints(db, module_id, constraint))
                    .collect();
                let kind_constraints = trait_def
                    .self_constraints()
                    .iter()
                    .find_map(|constraint| kind_constraints(constraint));

                AnnotatedType::SelfType {
                    trait_fql,
                    type_arity: kind_constraints.unwrap_or(0),
                    trait_constraints,
                }
            } else {
                // Self used outside trait/behavior context
                AnnotatedType::Missing
            }
        }

        hir::TypeReference::Named(path) => {
            resolve_named_type_annotation(db, module_id, path, type_idx)
        }

        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg = resolve_type_annotation(db, module_id, *arg_type);
            let ret = resolve_type_annotation(db, module_id, *return_type);
            AnnotatedType::Lambda {
                arg: Box::new(arg),
                ret: Box::new(ret),
            }
        }

        hir::TypeReference::Tuple(types) => {
            if types.is_empty() {
                AnnotatedType::Unit
            } else {
                let inner_types: Vec<_> = types
                    .iter()
                    .map(|t| resolve_type_annotation(db, module_id, *t))
                    .collect();
                // SAFETY: We checked that types is non-empty
                unsafe { AnnotatedType::Tuple(NonEmpty::new_unchecked(inner_types)) }
            }
        }

        hir::TypeReference::ParenthesizedType(inner) => {
            resolve_type_annotation(db, module_id, *inner)
        }

        hir::TypeReference::Bounded { base, args } => {
            let base_resolved = resolve_type_annotation(db, module_id, *base);
            let args_resolved: Vec<_> = args
                .iter()
                .map(|arg| resolve_type_annotation(db, module_id, *arg))
                .collect();

            AnnotatedType::Bounded {
                base: Box::new(base_resolved),
                args: args_resolved,
            }
        }
    }
}

fn trait_constraints(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    c: &hir::TypeVariableConstraint,
) -> Option<(Fql<Trait>, Name)> {
    match c {
        hir::TypeVariableConstraint::Trait(type_idx) => {
            let trait_fql = res::resolve_trait_by_ref_id(db, module_id, *type_idx).ok()?;
            let name = trait_fql.trait_name(db);
            Some((trait_fql, name))
        }
        hir::TypeVariableConstraint::Kind(_) => None,
    }
}

fn kind_constraints(c: &hir::TypeVariableConstraint) -> Option<usize> {
    match c {
        hir::TypeVariableConstraint::Trait(_) => None,
        hir::TypeVariableConstraint::Kind(arity) => Some(*arity),
    }
}

/// Resolve a Named type reference path to an AnnotatedType.
///
/// First tries to resolve as a type reference (finds TypeReference in target module),
/// then falls back to resolving as a type definition (TypeDefinition directly).
fn resolve_named_type_annotation(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    type_idx: hir::TypeIdx,
) -> AnnotatedType {
    // First try: resolve via type reference path
    if let Some(resolved_fql) = res::resolve_type_reference_by_path(db, current_module_id, path) {
        return resolve_type_annotation(db, resolved_fql.module_id, resolved_fql.local_id);
    }

    // Second try: resolve directly as a type definition
    if let Ok(td_fql) = res::resolve_type_definition_by_ref_id(db, current_module_id, type_idx) {
        return resolve_type_definition_to_annotated(db, td_fql.module_id, td_fql.local_id);
    }

    AnnotatedType::Missing
}

/// Convert a type definition to an AnnotatedType.
/// Handles type variables, single types, and union types.
fn resolve_type_definition_to_annotated(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    type_def_idx: hir::TypeDefinitionIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let hir::TypeDefinition { name, kind } = hir_module.get_type_definition(type_def_idx);

    match kind {
        hir::TypeDefinitionKind::Missing => AnnotatedType::Missing,
        hir::TypeDefinitionKind::TypeVariable(type_var) => {
            let fql = Fql::new(module_id, type_def_idx);
            match type_var {
                hir::TypeVariable::Unbound => AnnotatedType::TypeVar {
                    fql,
                    name: name.clone(),
                },
                hir::TypeVariable::Constrained(constraints) => {
                    let trait_constraints: Vec<_> = constraints
                        .iter()
                        .filter_map(|constraint| trait_constraints(db, module_id, constraint))
                        .collect();

                    NonEmpty::try_from(trait_constraints)
                        .map(|constraints| AnnotatedType::ConstrainedTypeVar {
                            fql: fql.clone(),
                            name: name.clone(),
                            constraints,
                        })
                        .unwrap_or_else(|_| AnnotatedType::TypeVar {
                            fql,
                            name: name.clone(),
                        })
                }
            }
        }
        hir::TypeDefinitionKind::Single(_) | hir::TypeDefinitionKind::Union(_) => {
            AnnotatedType::TypeDef(Fql::new(module_id, type_def_idx), name.clone())
        }
    }
}
