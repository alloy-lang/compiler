use crate::{EPTrFql, Fql, TypeResolutionError};
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
    ) -> TypeResolutionError;

    fn validate(
        db: &dyn hir::HirDatabase,
        source_ref: impl Into<EPTrFql>,
        item_fql: Fql<T>,
        subname: Option<hir::Name>,
    ) -> Option<TypeResolutionError>;
}

pub(crate) fn resolve_by_path<T, R>(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    source_ref: impl Into<EPTrFql> + Clone,
) -> Result<Fql<T>, TypeResolutionError>
where
    R: Resolver<T>,
{
    let (item_name, subname, scope, module_id, full_path) = match path {
        hir::Path::ThisModule {
            name,
            scope,
            subname,
        } => (
            name.clone(),
            subname.clone(),
            *scope,
            current_module_id,
            ne_vec![name.clone()],
        ),
        hir::Path::OtherModule(fqn) => {
            let module_slug = fqn.module_slug();
            let Some((item_name, subname, other_module_id)) = find_module_outer(db, fqn) else {
                return Err(TypeResolutionError::UnknownModule {
                    module_slug,
                    source_ref: source_ref.into(),
                });
            };

            (
                item_name,
                subname,
                Scopes::ROOT,
                other_module_id,
                fqn.segments(),
            )
        }
        hir::Path::Unknown(path) => {
            return Err(R::unknown_item_error(
                source_ref,
                current_module_id,
                path.clone(),
            ))
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

fn find_module_outer(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<(hir::Name, Option<hir::Name>, ModuleId)> {
    let module_slug = fqn.module_slug();
    if let Some(subname) = &fqn.sub_path {
        // Case 1: Has subname - direct lookup with sub-component
        let other_module_id = db.find_module_by_slug(&module_slug)?;
        Some((fqn.name.clone(), Some(subname.clone()), other_module_id))
    } else {
        // Case 2: No subname - try different splits to find the module boundary
        let (item_name, subname, other_module_id) = find_module(db, fqn)?;

        Some((item_name, subname, other_module_id))
    }
}

fn find_module(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<(hir::Name, Option<hir::Name>, ModuleId)> {
    let full_path: NonEmpty<_> = fqn.segments();
    let full_path_length = full_path.len().into();

    for split_point in (1..=full_path_length).rev() {
        let module_path = &full_path[..split_point];
        let item_name = if split_point < full_path_length {
            &full_path[split_point]
        } else {
            continue;
        };
        let remaining_path = &full_path[(split_point + 1)..];

        let module_slug = module_path
            .iter()
            .map(hir::Name::as_str)
            .collect::<Vec<_>>()
            .join("::");

        let other_module_id = db.find_module_by_slug(&module_slug)?;

        return Some((
            item_name.clone(),
            remaining_path.iter().next().cloned(),
            other_module_id,
        ));
    }

    None
}
