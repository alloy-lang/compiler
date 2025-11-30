use crate::hir_ty::ResolvedType;
use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;

/// Context for resolving type references, tracks type variable ID assignments
pub(super) struct TypeResolutionContext {
    /// Maps type definition indices to assigned Generic IDs
    type_var_to_id: FxHashMap<hir::TypeDefinitionIdx, usize>,
    /// Next generic ID to assign
    next_id: usize,
}

impl TypeResolutionContext {
    fn new() -> Self {
        Self {
            type_var_to_id: FxHashMap::default(),
            next_id: 0,
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
}

pub fn type_reference_to_resolved(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> ResolvedType {
    let mut ctx = TypeResolutionContext::new();
    type_reference_to_resolved_with_ctx(db, current_module_id, path, scope, &mut ctx)
}

fn type_reference_to_resolved_with_ctx(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
    ctx: &mut TypeResolutionContext,
) -> ResolvedType {
    if let Some((resolved_module_id, type_idx)) =
        resolve_type_reference_path(db, current_module_id, path, scope)
    {
        return resolve_type_reference(db, resolved_module_id, type_idx, scope, ctx);
    };

    ResolvedType::Unknown
}

fn resolve_type_reference(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    type_idx: hir::TypeIdx,
    scope: ScopeIdx,
    ctx: &mut TypeResolutionContext,
) -> ResolvedType {
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match &type_ref {
        hir::TypeReference::Unconstrained => ResolvedType::Unknown,
        hir::TypeReference::Missing => ResolvedType::Unknown,
        hir::TypeReference::SelfRef => ResolvedType::Unknown, // TODO: Handle self type
        hir::TypeReference::Unit => ResolvedType::Unit,
        hir::TypeReference::Named(path) => Some(type_reference_to_resolved_with_ctx(
            db,
            current_module_id,
            path,
            scope,
            ctx,
        ))
        .filter(|t| *t != ResolvedType::Unknown)
        .unwrap_or_else(|| {
            super::type_definition::type_definition_to_resolved(
                db,
                current_module_id,
                path,
                scope,
                ctx,
            )
        }),
        hir::TypeReference::BuiltIn(built_in) => ResolvedType::BuiltIn(*built_in),
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg = resolve_type_reference(db, current_module_id, *arg_type, scope, ctx);
            let ret = resolve_type_reference(db, current_module_id, *return_type, scope, ctx);
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
                        .map(|t| resolve_type_reference(db, current_module_id, *t, scope, ctx))
                        .collect();
                    ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types))
                }
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            resolve_type_reference(db, current_module_id, *inner, scope, ctx)
        }
        hir::TypeReference::Bounded { base, args } => {
            // Resolve the base type (e.g., List, Option, Test)
            let base_resolved = resolve_type_reference(db, current_module_id, *base, scope, ctx);

            // Resolve each type argument
            let args_resolved: Vec<_> = args
                .iter()
                .map(|arg| resolve_type_reference(db, current_module_id, *arg, scope, ctx))
                .collect();

            ResolvedType::Bounded {
                base: Box::new(base_resolved),
                args: args_resolved,
            }
        }
    }
}

fn resolve_type_reference_path(
    db: &dyn HirTyDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Option<(ModuleId, hir::TypeIdx)> {
    match path {
        hir::Path::ThisModule {
            path: this_path,
            scope: _,
        } => {
            let type_idx =
                get_type_reference_by_name(db, current_module_id, this_path.first(), scope)?;
            Some((current_module_id, type_idx))
        }
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module.iter().map(|n| n.as_str()).join("::");
            let other_module_id = db.find_module_by_slug(&*module_slug)?;
            let type_idx =
                get_type_reference_by_name(db, other_module_id, &fqn.name, Scopes::ROOT)?;
            Some((other_module_id, type_idx))
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
