//! Generic cross-module resolution utilities
//!
//! This module provides a generic framework for resolving cross-module references
//! for different HIR node types (expressions, type definitions, traits, etc.)

use crate::{EPTrFql, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;

/// Trait for looking up items by name in a HIR module
///
/// Generic parameter T is the underlying HIR type (e.g., Expression, TypeDefinition)
/// not the Idx type (e.g., ExpressionIdx)
pub(crate) trait ModuleLookup<T> {
    /// The type of the item returned alongside the index
    type Item;

    /// Look up an item by name in the given module
    /// Returns Some((item_idx, item)) if found, None otherwise
    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<T>, Self::Item)>;

    /// Validate the found item (e.g., check if type has a variant)
    /// Returns true if validation passes, false otherwise
    fn validate(_item: Self::Item, _remaining_path: &[hir::Name]) -> bool {
        true // Default: no validation needed
    }

    /// Create an error for when the module is not found
    fn unknown_module_error(
        module_slug: String,
        source_ref: impl Into<EPTrFql>,
    ) -> TypeResolutionError {
        TypeResolutionError::UnknownModule {
            module_slug,
            source_ref: source_ref.into(),
        }
    }

    /// Create an error for when the item is not found in the module
    fn unknown_item_error(
        source_ref: impl Into<EPTrFql>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    ) -> TypeResolutionError {
        match source_ref.into() {
            EPTrFql::Expression(fql) => TypeResolutionError::UnknownExpressionReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::Pattern(fql) => TypeResolutionError::UnknownPatternReference {
                source_ref: fql,
                module_id,
                path,
            },
            EPTrFql::TypeReference(fql) => TypeResolutionError::UnknownTypeReference {
                source_ref: fql,
                module_id,
                path,
            },
        }
    }
}

/// Generic cross-module resolution function
pub(crate) fn resolve_cross_module<T, L>(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: impl Into<EPTrFql> + Clone,
) -> Result<Fql<T>, TypeResolutionError>
where
    L: ModuleLookup<T>,
{
    let module_slug = fqn.module_slug();

    // Case 1: No sub_path - direct lookup
    if fqn.sub_path.is_some() {
        let Some(other_module_id) = db.find_module_by_slug(&module_slug) else {
            return Err(L::unknown_module_error(
                module_slug.to_string(),
                source_ref.clone(),
            ));
        };

        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let Some((item_id, item)) = L::lookup_in_module(&hir_module, &fqn.name) else {
            return Err(L::unknown_item_error(
                source_ref,
                other_module_id,
                fqn.segments(),
            ));
        };

        // Validate even for direct lookups (in case there's a sub-component)
        if !L::validate(item, &[]) {
            return Err(L::unknown_item_error(
                source_ref,
                other_module_id,
                fqn.segments(),
            ));
        }

        return Ok(Fql::new(other_module_id, item_id));
    }

    // Case 2: Has sub_path - try different splits
    let Some((item_name, _remaining_path, other_module_id)) = find_module(db, fqn) else {
        return Err(L::unknown_module_error(module_slug.to_string(), source_ref));
    };
    let (hir_module, _) = hir::lower_file(db, other_module_id);
    let Some((item_id, _item)) = L::lookup_in_module(&hir_module, &item_name) else {
        return Err(L::unknown_item_error(
            source_ref,
            other_module_id,
            fqn.segments(),
        ));
    };

    Ok(Fql::new(other_module_id, item_id))
}

/// Generic cross-module resolution that returns Option instead of Result
pub(crate) fn resolve_cross_module_optional<T, L>(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<Fql<T>>
where
    L: ModuleLookup<T>,
{
    let module_slug = fqn.module_slug();

    // Case 1: No sub_path - direct lookup
    if fqn.sub_path.is_some() {
        let other_module_id = db.find_module_by_slug(&module_slug)?;
        let (hir_module, _) = hir::lower_file(db, other_module_id);
        let (item_id, item) = L::lookup_in_module(&hir_module, &fqn.name)?;
        if !L::validate(item, &[]) {
            return None;
        }
        return Some(Fql::new(other_module_id, item_id));
    }

    // Case 2: Has sub_path - try different splits
    let (item_name, remaining_path, other_module_id) = find_module(db, fqn)?;
    let (hir_module, _) = hir::lower_file(db, other_module_id);
    let (item_id, item) = L::lookup_in_module(&hir_module, &item_name)?;

    if !L::validate(item, &remaining_path) {
        return None;
    }

    Some(Fql::new(other_module_id, item_id))
}

fn find_module(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
) -> Option<(hir::Name, Vec<hir::Name>, ModuleId)> {
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
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join("::");

        let other_module_id = db.find_module_by_slug(&module_slug)?;

        return Some((item_name.clone(), remaining_path.into(), other_module_id));
    }

    None
}
