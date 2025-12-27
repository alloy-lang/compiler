mod behavior;
mod cross_module_resolver;
mod diagnostics;
mod expr;
mod fql;
mod pattern;
mod r#trait;
mod type_definition;
mod type_reference;
mod type_variable;

use alloy_hir as hir;
use alloy_scope::Scopes;
pub use behavior::*;
pub use diagnostics::*;
pub use expr::*;
pub use fql::*;
use la_arena::Idx;
pub use pattern::*;
pub use r#trait::*;
pub use type_definition::*;
pub use type_reference::*;
pub use type_variable::*;

// ============================================================================
// Expression Lookup
// ============================================================================

struct ExpressionLookup;

impl cross_module_resolver::ModuleLookup<hir::Expression> for ExpressionLookup {
    type Item = hir::Expression;

    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<hir::Expression>, Self::Item)> {
        hir_module
            .get_expression_by_name(name, Scopes::ROOT)
            .map(|(id, expr)| (id, expr.clone()))
    }
}

/// Resolve a cross-module expression reference
pub fn resolve_cross_module_expression(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: Fql<hir::Expression>,
) -> Result<Fql<hir::Expression>, TypeResolutionError> {
    cross_module_resolver::resolve_cross_module::<hir::Expression, ExpressionLookup>(
        db, fqn, source_ref,
    )
}

// ============================================================================
// Type Definition Lookup
// ============================================================================

struct TypeDefinitionLookup;

impl cross_module_resolver::ModuleLookup<hir::TypeDefinition> for TypeDefinitionLookup {
    type Item = hir::TypeDefinition;

    fn lookup_in_module(
        hir_module: &hir::HirModule,
        name: &hir::Name,
    ) -> Option<(Idx<hir::TypeDefinition>, Self::Item)> {
        hir_module
            .get_type_definition_by_name(name, Scopes::ROOT)
            .map(|(id, typedef)| (id, typedef.clone()))
    }

    fn validate(item: Self::Item, remaining_path: &[hir::Name]) -> bool {
        // For type definitions, check if the variant exists (if one is requested)
        if let Some(variant_name) = remaining_path.last() {
            return item.kind.has_variant(variant_name);
        }
        true
    }
}

/// Resolve a cross-module type definition reference
///
/// This handles qualified variant references by trying different ways to split
/// the path into (module, type, variant).
///
/// For example, `std::option::Option::Some` could be split as:
/// - module: "std::option", type: "Option", variant: "Some"
/// - module: "std", type: "option", variant: "Option" (invalid - Option is not a variant)
pub(crate) fn resolve_cross_module_type_definition(
    db: &dyn hir::HirDatabase,
    fqn: &hir::Fqn,
    source_ref: EPTrFql,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    cross_module_resolver::resolve_cross_module::<hir::TypeDefinition, TypeDefinitionLookup>(
        db, fqn, source_ref,
    )
}
