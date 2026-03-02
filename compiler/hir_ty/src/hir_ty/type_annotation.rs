use super::type_definition;
use crate::hir_ty::{Fql, ResolvedType};
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_hir_resolved as res;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;

/// Context for resolving type references, tracks type variable ID assignments
pub struct TypeResolutionContext {
    /// Maps type definition indices to assigned Generic IDs
    type_var_to_id: FxHashMap<hir::TypeDefinitionIdx, usize>,
    /// Next generic ID to assign
    next_id: usize,
    /// Cached generic ID for Self type (ensures consistent ID across multiple Self references)
    self_generic_id: Option<usize>,
}

impl TypeResolutionContext {
    pub fn new() -> Self {
        Self {
            type_var_to_id: FxHashMap::default(),
            next_id: 0,
            self_generic_id: None,
        }
    }

    /// Get or assign a Generic ID for a type variable
    pub(super) fn get_or_assign_id(&mut self, type_def_idx: hir::TypeDefinitionIdx) -> usize {
        if let Some(&id) = self.type_var_to_id.get(&type_def_idx) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.type_var_to_id.insert(type_def_idx, id);
        id
    }

    /// Get or assign a Generic ID for the Self type variable
    fn get_or_assign_self_id(&mut self) -> usize {
        if let Some(id) = self.self_generic_id {
            return id;
        }
        let id = self.next_id;
        self.next_id += 1;
        self.self_generic_id = Some(id);
        id
    }
}

fn named_type_reference_to_resolved_with_ctx(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    ctx: &mut TypeResolutionContext,
) -> Option<ResolvedType> {
    let resolved_type_fql = res::resolve_type_reference_by_path(db, current_module_id, path)?;
    type_reference_to_resolved_type(
        db,
        resolved_type_fql.module_id,
        resolved_type_fql.local_id,
        ctx,
    )
}

pub fn type_reference_to_resolved_type(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeIdx,
    ctx: &mut TypeResolutionContext,
) -> Option<ResolvedType> {
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    let ty = match &type_ref {
        hir::TypeReference::Unconstrained => ResolvedType::Unconstrained,
        hir::TypeReference::Missing => ResolvedType::Missing,
        hir::TypeReference::SelfRef(scope) => {
            // Check if we're inside a behavior — Self resolves to the concrete attached_type
            if let Some((_idx, behavior)) = hir_module.find_behavior_containing_scope(*scope) {
                type_reference_to_resolved_type(db, current_module_id, behavior.attached_type, ctx)?
            }
            // Check if we're inside a trait — Self is a constrained generic
            else if let Some((_idx, trait_def)) = hir_module.find_trait_containing_scope(*scope) {
                let generic_id = ctx.get_or_assign_self_id();

                let trait_constraints: Vec<_> = trait_def
                    .self_constraints()
                    .iter()
                    .filter_map(|c| match c {
                        hir::TypeVariableConstraint::Trait(type_idx) => {
                            let trait_fql =
                                res::resolve_trait_by_ref_id(db, current_module_id, *type_idx)
                                    .ok()?;
                            let name = trait_fql.trait_name(db);
                            Some((trait_fql, name))
                        }
                        hir::TypeVariableConstraint::Kind(_) => None,
                    })
                    .collect();

                if trait_constraints.is_empty() {
                    ResolvedType::Generic(generic_id)
                } else {
                    unsafe {
                        ResolvedType::ConstrainedGeneric {
                            id: generic_id,
                            constraints: NonEmpty::new_unchecked(trait_constraints),
                        }
                    }
                }
            } else {
                // Self used outside trait/behavior context
                ResolvedType::Missing
            }
        }
        hir::TypeReference::Unit => ResolvedType::Unit,
        hir::TypeReference::Named(path) => named_type_reference_to_resolved_with_ctx(
            db,
            current_module_id,
            path,
            ctx,
        )
        .or_else(|| {
            type_definition::type_definition_to_resolved(db, ctx, current_module_id, type_idx)
        })?,
        hir::TypeReference::BuiltIn(built_in) => ResolvedType::BuiltIn(*built_in),
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg = type_reference_to_resolved_type(db, current_module_id, *arg_type, ctx);
            let ret = type_reference_to_resolved_type(db, current_module_id, *return_type, ctx);
            arg.zip(ret).map(|(a, r)| ResolvedType::Lambda {
                arg_type: Box::new(a),
                return_type: Box::new(r),
            })?
        }
        hir::TypeReference::Tuple(types) => {
            if types.is_empty() {
                ResolvedType::Unit
            } else {
                unsafe {
                    let inner_types: Vec<_> = types
                        .iter()
                        .map(|t| {
                            type_reference_to_resolved_type(db, current_module_id, *t, ctx)
                                .unwrap_or(ResolvedType::UnknownReference(Fql::new(
                                    current_module_id,
                                    *t,
                                )))
                        })
                        .collect();
                    ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types))
                }
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            type_reference_to_resolved_type(db, current_module_id, *inner, ctx)?
        }
        hir::TypeReference::Bounded { base, args } => {
            // Resolve the base type (e.g., List, Option, Test)
            let base_resolved = type_reference_to_resolved_type(db, current_module_id, *base, ctx)?;

            // Resolve each type argument
            let args_resolved: Vec<_> = args
                .iter()
                .map(|arg| {
                    type_reference_to_resolved_type(db, current_module_id, *arg, ctx).unwrap_or(
                        ResolvedType::UnknownReference(Fql::new(current_module_id, *arg)),
                    )
                })
                .collect();

            ResolvedType::Bounded {
                base: Box::new(base_resolved),
                args: args_resolved,
            }
        }
    };

    Some(ty)
}
