use crate::{EPTrFql, Fql, HirResolutionError};
use alloy_hir as hir;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::{ne_vec, NonEmpty};

pub(crate) trait Resolver<T> {
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
        scope: ScopeIdx,
    ) -> Option<(Idx<T>, T)>;

    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> HirResolutionError;

    fn validate(
        db: &dyn hir::HirDatabase,
        source_ref: impl Into<EPTrFql>,
        item_fql: Fql<T>,
        subname: Option<hir::Name>,
    ) -> Option<HirResolutionError>;
}

pub(crate) fn resolve_by_path<T, R>(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    source_ref: impl Into<EPTrFql>,
) -> Result<Fql<T>, HirResolutionError>
where
    R: Resolver<T>,
{
    let (item_name, subname, scope, module_id, full_path) = match path {
        hir::Path::ThisModule {
            name,
            scope,
            subname,
            ..
        } => (
            name.clone(),
            subname.clone(),
            *scope,
            current_module_id,
            ne_vec![name.clone()],
        ),
        hir::Path::OtherModule(fqn, _resolution_kinds) => (
            fqn.name.clone(),
            fqn.sub_path.clone(),
            Scopes::ROOT,
            fqn.module_id,
            fqn.segments(),
        ),
        hir::Path::UnknownReference(path) => {
            return Err(R::unknown_item_error(
                source_ref,
                current_module_id,
                path.clone(),
            ))
        }
        hir::Path::UnresolvedModule(err) => {
            return Err(HirResolutionError::UnresolvedModule {
                err: err.clone(),
                source_ref: source_ref.into(),
            });
        }
    };

    let (hir_module, _) = hir::lower_file(db, module_id);

    let Some((item_idx, _item)) = R::lookup_in_module(&hir_module, &item_name, scope) else {
        return Err(R::unknown_item_error(source_ref, module_id, full_path));
    };

    // Validate any remaining path components
    if let Some(err) = R::validate(db, source_ref, Fql::new(module_id, item_idx), subname) {
        return Err(err);
    }

    Ok(Fql::new(module_id, item_idx))
}
