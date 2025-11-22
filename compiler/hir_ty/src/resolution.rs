//! Cross-module symbol resolution for the Alloy compiler
//!
//! This module provides functionality to resolve imports and symbols across module boundaries.
//!
//! # Overview
//!
//! The resolution system has three main components:
//!
//! 1. **Module Exports** - Collecting symbols exported from a module
//! 2. **Import Resolution** - Mapping import statements to actual files
//! 3. **Symbol Resolution** - Looking up symbols in imported modules
//!
//! # Example Usage
//!
//! ```ignore
//! use alloy_hir_typed::resolution::*;
//!
//! // Given a file and workspace
//! let exports = module_exports(db, file);
//!
//! // Check what types are exported
//! if let Some(type_idx) = exports.types.get(&type_name) {
//!     // Type is exported from this module
//! }
//!
//! // Resolve an import
//! let imported_file = resolve_import(db, current_file, import, workspace)?;
//!
//! // Look up a symbol in the imported module
//! let symbol = resolve_cross_module_symbol(db, imported_file, &symbol_name)?;
//! ```
//!
//! # Salsa Queries
//!
//! The following functions are cached by Salsa:
//! - `module_exports()` - Caches exported symbols per file
//! - `resolve_import()` - Caches import resolution
//! - `workspace_files()` - Caches workspace file list
//!
//! # Current Limitations
//!
//! - Only exports symbols at root scope (scope 0)
//! - Import resolution is relative to the importing file's directory
//! - No support for external packages yet
//! - No re-exports or visibility modifiers

use alloy_hir as hir;
use alloy_workspace::{RawSourceFile, Workspace};

use crate::HirTyDatabase;

// Re-export ModuleExports from hir
pub use hir::ModuleExports;

/// Collect all exported symbols from a module
/// For now, we export everything at the root scope (scope 0)
#[salsa::tracked]
pub fn module_exports(db: &dyn HirTyDatabase, file: RawSourceFile) -> ModuleExports {
    let (hir_module, _parse_errors) = hir::lower_file(db, file);
    hir_module.module_exports()
}

/// Get all files in the workspace
/// This is a helper to access workspace files from the database
#[salsa::tracked]
pub fn workspace_files(
    db: &dyn HirTyDatabase,
    workspace: Workspace,
) -> Vec<(String, RawSourceFile)> {
    workspace
        .files(db)
        .iter()
        .map(|(slug, file)| (slug.clone(), *file))
        .collect()
}

/// Resolve an import to the file it references
/// Returns None if the import cannot be resolved
#[salsa::tracked]
pub fn resolve_import(
    db: &dyn HirTyDatabase,
    file: RawSourceFile,
    import: hir::Import,
    workspace: Workspace,
) -> Option<RawSourceFile> {
    // Get the import path segments
    let segments = import.segments();
    let last = import.last();

    // Build the import path: foo.bar.baz -> foo/bar/baz.alloy
    let current_path = file.raw_path(db);
    let current_dir = std::path::Path::new(current_path.as_ref())
        .parent()
        .unwrap_or(std::path::Path::new(""));

    // Build the target path from segments
    let mut target_path = current_dir.to_path_buf();
    for segment in segments {
        target_path.push(segment.as_str());
    }
    target_path.push(format!("{}.alloy", last.as_str()));

    let target_path_str = target_path.to_str()?.to_string();

    // Find a file in the workspace that matches this path
    let files = workspace_files(db, workspace);

    for (slug, source_file) in files {
        let file_path = source_file.raw_path(db);

        // Check if this file matches the target path
        // We need to handle both absolute and relative paths
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
        files.insert("types".to_string(), types_file);
        files.insert("main".to_string(), main_file);
        let workspace = Workspace::new(&db, files);

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
        let resolved_file = crate::resolve_import(&db, main_file, types_import, workspace);
        assert!(
            resolved_file.is_some(),
            "Expected import 'types' to resolve to types.alloy"
        );
        assert!(
            resolved_file.unwrap() == types_file,
            "Expected import to resolve to the types.alloy file"
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
    }
}
