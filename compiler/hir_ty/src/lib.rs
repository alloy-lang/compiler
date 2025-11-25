use alloy_hir as hir;
use alloy_workspace::ModuleId;
use std::os::unix::raw::mode_t;

mod hir_ty;
use hir_ty::*;

mod diagnostics;
mod resolution;
use diagnostics::*;

#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirTyDatabase: hir::HirDatabase {}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypedModule {
    warnings: Vec<TypeInferenceWarning>,
    errors: Vec<TypeInferenceError>,
}

impl HirTypedModule {
    pub(crate) fn empty() -> Self {
        Self {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResolutionResult {
    resolved_type: Option<ResolvedType>,
    warnings: Vec<TypeInferenceWarning>,
    errors: Vec<TypeInferenceError>,
}

/// type checking for everything in a module
/// stores resolved types for all module symbols, regardless of scope
/// for the LSP implementation, we will want to generate errors and warnings for the current file
/// during full compilation, we will want to generate errors and warnings for all modules
#[salsa::tracked]
pub fn type_check_module(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    todo!()
}

#[salsa::tracked]
pub fn find_expression_type(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    expression_id: hir::ExpressionIdx,
) -> TypeResolutionResult {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expression = hir_module.get_expression(expression_id);
    match expression {
        _ => todo!(),
    }

    todo!()
}

#[salsa::tracked]
pub fn find_pattern_type(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    pattern_id: hir::PatternIdx,
) -> TypeResolutionResult {
    todo!()
}
