//! Cross-module symbol resolution for the Alloy compiler
//!
//! This module provides functionality to resolve imports and symbols across module boundaries.
//!
//! # Overview
//!
//! The resolution system has several main components:
//!
//! 1. **Module Exports** - Collecting symbols exported from a module
//! 2. **Module Resolution** - Mapping ModuleId to actual files
//! 3. **Import Resolution** - Resolving import statements (which may include specific exports)
//! 4. **Symbol Resolution** - Looking up symbols in imported modules
//!
//! # Module Identifiers
//!
//! Modules are identified using Rust-style `::` syntax:
//! - `std::collections` maps to `std/collections.alloy`
//! - `foo::bar::baz` maps to `foo/bar/baz.alloy`
//!
//! # Import Forms
//!
//! Two forms of imports are supported:
//! 1. **Module import**: `import std::collections` - imports the entire module
//! 2. **Specific export**: `import std::collections::HashMap` - imports only HashMap
//!
//! # Example Usage
//!
//! ```ignore
//! use alloy_hir_typed::resolution::*;
//!
//! // Resolve an import statement
//! let resolved = resolve_import(db, current_file, import, workspace)?;
//!
//! match resolved {
//!     ResolvedImport::Module(module_id) => {
//!         // Entire module imported
//!         let exports = module_exports_by_id(db, module_id, workspace)?;
//!     }
//!     ResolvedImport::Symbol { module_id, symbol_name } => {
//!         // Specific symbol imported
//!         let symbol = resolve_symbol_in_module(db, module_id, &symbol_name, workspace)?;
//!     }
//! }
//! ```
//!
//! # Salsa Queries
//!
//! The following functions are cached by Salsa:
//! - `module_exports()` - Caches exported symbols per file
//! - `resolve_module_id()` - Caches ModuleId -> RawSourceFile resolution
//! - `workspace_files()` - Caches workspace file list
//!
//! # Current Limitations
//!
//! - Only exports symbols at root scope (scope 0)
//! - Import resolution is relative to the importing file's directory
//! - No support for external packages yet
//! - No re-exports or visibility modifiers

use alloy_hir as hir;
use alloy_workspace::{ModuleId, RawSourceFile, Workspace};
use std::sync::Arc;

use crate::HirTyDatabase;

// Re-export ModuleExports from hir
pub use hir::ModuleExports;

/// Represents the result of resolving an import statement
/// The presence of a ResolvedImport guarantees that the module exists and was found
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedImport {
    /// The import refers to an entire module (e.g., "import std::collections")
    /// Stores the validated module path (e.g., "std::collections")
    Module(Arc<str>),
    /// The import refers to a specific symbol in a module (e.g., "import std::collections::HashMap")
    /// The module path is guaranteed to exist in the workspace
    Symbol {
        /// The validated module path
        module_path: Arc<str>,
        symbol_name: hir::Name,
    },
}

impl ResolvedImport {
    /// Convert the module path to a ModuleId
    /// Since ResolvedImport guarantees the module was validated, this ModuleId is known to exist
    pub fn to_module_id<'db>(&self, db: &'db dyn crate::HirTyDatabase) -> ModuleId<'db> {
        match self {
            ResolvedImport::Module(path) => ModuleId::new(db, path.clone()),
            ResolvedImport::Symbol { module_path, .. } => ModuleId::new(db, module_path.clone()),
        }
    }

    /// Get the symbol name if this is a specific symbol import
    pub fn symbol_name(&self) -> Option<&hir::Name> {
        match self {
            ResolvedImport::Module(_) => None,
            ResolvedImport::Symbol { symbol_name, .. } => Some(symbol_name),
        }
    }
}

/// Collect all exported symbols from a module file
/// For now, we export everything at the root scope (scope 0)
#[salsa::tracked]
pub fn module_exports(db: &dyn HirTyDatabase, file: RawSourceFile) -> ModuleExports {
    let (hir_module, _parse_errors) = hir::lower_file(db, file);
    hir_module.module_exports()
}

/// Resolve a ModuleId to its source file in the workspace
/// First tries direct lookup, then tries relative resolution from current file
#[salsa::tracked]
pub fn resolve_module_id<'db>(
    db: &'db dyn HirTyDatabase,
    module_id: ModuleId<'db>,
    workspace: Workspace<'db>,
    current_file: RawSourceFile,
) -> Option<RawSourceFile> {
    // First, try direct lookup in workspace
    if let Some(file) = workspace.get_file(module_id) {
        return Some(file);
    }

    // If not found, try relative resolution
    // Build the file path and search for matching files
    let module_file_path = module_id.file_path(db);
    let current_path = current_file.raw_path(db);
    let current_dir = std::path::Path::new(current_path.as_ref())
        .parent()
        .unwrap_or(std::path::Path::new(""));

    let target_path = current_dir.join(&module_file_path);
    let target_path_str = target_path.to_str()?.to_string();

    // Search through all modules to find one with matching file path
    let modules = workspace_modules(&workspace);

    for (_module_id, source_file) in modules {
        let file_path = source_file.raw_path(db);

        if file_path.as_ref() == target_path_str
            || file_path.ends_with(&target_path_str)
            || std::path::Path::new(file_path.as_ref())
                .file_name()
                .and_then(|n| n.to_str())
                == std::path::Path::new(&target_path_str)
                .file_name()
                .and_then(|n| n.to_str())
        {
            return Some(source_file);
        }
    }

    None
}

/// Get exports from a module identified by ModuleId
pub fn module_exports_by_id<'db>(
    db: &'db dyn HirTyDatabase,
    module_id: ModuleId<'db>,
    workspace: Workspace,
    current_file: RawSourceFile,
) -> Option<ModuleExports> {
    let file = resolve_module_id(db, module_id, workspace, current_file)?;
    Some(module_exports(db, file))
}

/// Resolve a symbol in a specific module
pub fn resolve_symbol_in_module<'db>(
    db: &'db dyn HirTyDatabase,
    module_id: ModuleId<'db>,
    symbol_name: &hir::Name,
    workspace: Workspace,
    current_file: RawSourceFile,
) -> Option<ResolvedSymbol> {
    let file = resolve_module_id(db, module_id, workspace, current_file)?;
    resolve_cross_module_symbol(db, file, symbol_name)
}

/// Get all modules in the workspace
/// This is a helper to access workspace modules
pub fn workspace_modules<'db>(
    workspace: &Workspace<'db>,
) -> Vec<(ModuleId<'db>, RawSourceFile)> {
    workspace
        .files()
        .iter()
        .map(|(module_id, file)| (*module_id, *file))
        .collect()
}

/// Resolve an import statement, which may be either:
/// - A module import: "import std::collections" -> ResolvedImport::Module
/// - A specific export: "import std::collections::HashMap" -> ResolvedImport::Symbol
///
/// The resolution algorithm:
/// 1. Try to resolve all segments as a module path
/// 2. If that fails and there are multiple segments, try resolving all-but-last as module
///    and last as a specific export
///
/// Returns None if the module cannot be found in the workspace
/// The returned ResolvedImport guarantees the module exists and was validated
#[salsa::tracked]
pub fn resolve_import<'db>(
    db: &'db dyn HirTyDatabase,
    current_file: RawSourceFile,
    import: hir::Import,
    workspace: Workspace<'db>,
) -> Option<ResolvedImport> {
    // First, try to resolve the entire import as a module
    let module_id = import.to_module_id(db);
    let module_path = module_id.path(db).clone();

    if resolve_module_id(db, module_id, workspace.clone(), current_file).is_some() {
        return Some(ResolvedImport::Module(module_path));
    }

    // If that fails, try splitting as module + export (if possible)
    if let Some((parent_module_id, export_name)) = import.try_split_export(db) {
        if resolve_module_id(db, parent_module_id, workspace, current_file).is_some() {
            let parent_path = parent_module_id.path(db).clone();
            return Some(ResolvedImport::Symbol {
                module_path: parent_path,
                symbol_name: export_name,
            });
        }
    }

    // Could not resolve the import
    None
}

/// Resolve a cross-module reference
/// Given an import and a symbol name, find the symbol in the imported module
pub fn resolve_cross_module_symbol(
    db: &dyn HirTyDatabase,
    imported_file: RawSourceFile,
    symbol_name: &hir::Name,
) -> Option<ResolvedSymbol> {
    let exports = module_exports(db, imported_file);

    // Check if it's a type
    if let Some(type_idx) = exports.types.get(symbol_name) {
        return Some(ResolvedSymbol::Type {
            file: imported_file,
            idx: *type_idx,
        });
    }

    // Check if it's a trait
    if let Some(trait_idx) = exports.traits.get(symbol_name) {
        return Some(ResolvedSymbol::Trait {
            file: imported_file,
            idx: *trait_idx,
        });
    }

    // Check if it's an expression
    if let Some(expr_idx) = exports.expressions.get(symbol_name) {
        return Some(ResolvedSymbol::Expression {
            file: imported_file,
            idx: *expr_idx,
        });
    }

    None
}

/// Represents a resolved symbol that may be in any module
#[derive(Clone, PartialEq, Eq)]
pub enum ResolvedSymbol {
    Type {
        file: RawSourceFile,
        idx: hir::TypeDefinitionIdx,
    },
    Trait {
        file: RawSourceFile,
        idx: hir::TraitIdx,
    },
    Expression {
        file: RawSourceFile,
        idx: hir::ExpressionIdx,
    },
}

#[cfg(test)]
mod tests {
    use crate::tests::TestHirTyDatabase;
    use alloy_hir as hir;

    #[test]
    fn test_cross_module_resolution() {
        use alloy_workspace::{RawSourceFile, Workspace};
        use std::collections::HashMap;
        use std::sync::Arc;

        let db = TestHirTyDatabase::default();

        // File 1: types.alloy - exports a type definition
        let types_content = r#"
typedef Point =
  | Point(x: Int, y: Int)
end

let origin = Point(0, 0)
"#;
        let types_file = RawSourceFile::new(
            &db,
            Arc::from("/test/types.alloy"),
            Arc::from(types_content),
        );

        // File 2: main.alloy - imports from types.alloy
        let main_content = r#"
import types

let p = types.Point(1, 2)
"#;
        let main_file =
            RawSourceFile::new(&db, Arc::from("/test/main.alloy"), Arc::from(main_content));

        // Create a workspace with both files
        let mut files = HashMap::new();
        let types_module_id = alloy_workspace::ModuleId::new(&db, Arc::from("types"));
        let main_module_id = alloy_workspace::ModuleId::new(&db, Arc::from("main"));
        files.insert(types_module_id, types_file);
        files.insert(main_module_id, main_file);
        let workspace = Workspace::new(files);

        // Test 1: module_exports should collect Point and origin from types.alloy
        let types_exports = crate::module_exports(&db, types_file);

        // Check that Point type is exported
        let point_name = hir::Name::new("Point");
        assert!(
            types_exports.types.contains_key(&point_name),
            "Expected Point type to be exported from types.alloy"
        );

        // Check that origin expression is exported
        let origin_name = hir::Name::new("origin");
        assert!(
            types_exports.expressions.contains_key(&origin_name),
            "Expected origin expression to be exported from types.alloy"
        );

        // Test 2: resolve_import should find types.alloy from main.alloy
        let (main_hir, _) = hir::lower_file(&db, main_file);

        // Get the import from main.alloy
        let imports: Vec<_> = main_hir.imports().map(|(_, import, _, _)| import).collect();
        assert_eq!(
            imports.len(),
            1,
            "Expected exactly one import in main.alloy"
        );

        let types_import = imports[0].clone();
        let resolved = crate::resolve_import(&db, main_file, types_import.clone(), workspace.clone());
        assert!(
            resolved.is_some(),
            "Expected import 'types' to resolve"
        );

        // Should resolve as a Module import (not a specific symbol)
        let resolved_import = resolved.unwrap();
        match &resolved_import {
            crate::ResolvedImport::Module(module_path) => {
                // Verify the module path
                assert_eq!(
                    module_path.as_ref(),
                    "types",
                    "Expected module path to be 'types'"
                );
            }
            _ => panic!("Expected import to resolve as Module, not Symbol"),
        }

        // Convert to ModuleId and verify it resolves to the correct file
        let module_id = resolved_import.to_module_id(&db);
        let resolved_file = crate::resolve_module_id(&db, module_id, workspace, main_file);
        assert!(
            resolved_file.is_some(),
            "Expected module to resolve to a file"
        );
        assert!(
            resolved_file.unwrap() == types_file,
            "Expected module to resolve to types.alloy"
        );

        // Test 3: resolve_cross_module_symbol should find Point in types.alloy
        let resolved_symbol = crate::resolve_cross_module_symbol(&db, types_file, &point_name);
        assert!(
            resolved_symbol.is_some(),
            "Expected to resolve Point symbol from types.alloy"
        );

        match resolved_symbol.unwrap() {
            crate::ResolvedSymbol::Type { file, idx } => {
                assert!(
                    file == types_file,
                    "Expected symbol to come from types.alloy"
                );
                // Verify we can get the actual type definition
                let (types_hir, _) = hir::lower_file(&db, types_file);
                let _type_def = types_hir.get_type_definition(idx);
                // Type definition exists if we got here without panic
            }
            _ => panic!("Expected Point to resolve as a Type"),
        }

        // Test 4: resolve_cross_module_symbol should find origin expression in types.alloy
        let resolved_symbol = crate::resolve_cross_module_symbol(&db, types_file, &origin_name);
        assert!(
            resolved_symbol.is_some(),
            "Expected to resolve origin symbol from types.alloy"
        );

        match resolved_symbol.unwrap() {
            crate::ResolvedSymbol::Expression { file, .. } => {
                assert!(
                    file == types_file,
                    "Expected symbol to come from types.alloy"
                );
            }
            _ => panic!("Expected origin to resolve as an Expression"),
        }

        // Note: Testing specific symbol imports (like "import types::Point") would require
        // creating Import instances directly, but Import::new is private.
        // This functionality will be tested through integration tests with actual source code.
    }
}
