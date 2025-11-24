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

use crate::HirTyDatabase;
use alloy_hir as hir;
use alloy_workspace::{ModuleId, RawSourceFile, SourceFile, Workspace};
use rustc_hash::FxHashMap;

// Re-export ModuleExports from hir
pub use hir::ModuleExports;

/// Represents the result of resolving an import statement
/// The presence of a ResolvedImport guarantees that the module exists and was found
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedImport {
    /// The import refers to an entire module (e.g., "import std::collections")
    /// Stores the validated module path (e.g., "std::collections")
    Module(ModuleId),
    /// The import refers to a specific symbol in a module (e.g., "import std::collections::HashMap")
    /// The module path is guaranteed to exist in the workspace
    Symbol {
        /// The validated module path
        module_id: ModuleId,
        symbol_name: hir::Name,
    },
}

impl ResolvedImport {
    /// Convert the module path to a ModuleId
    /// Since ResolvedImport guarantees the module was validated, this ModuleId is known to exist
    pub fn to_module_id(&self) -> ModuleId {
        match self {
            ResolvedImport::Module(module_id) => *module_id,
            ResolvedImport::Symbol { module_id, .. } => *module_id,
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

pub fn resolve_imports<'db>(
    db: &'db dyn HirTyDatabase,
    current_module_id: ModuleId,
) -> FxHashMap<hir::Name, ResolvedImport> {
    let mut imports = FxHashMap::default();

    let current_file = db.get_source(current_module_id);
    let current_file = match current_file {
        SourceFile::Raw(raw) => raw,
        SourceFile::Virtual(_) => return FxHashMap::default(),
    };

    let (hir_module, _) = hir::lower_file(db, *current_file);

    for (_, import, _, _) in hir_module.imports() {
        let Some(resolved) = resolve_import(db, import) else {
            continue;
        };

        imports.insert(import.last().clone(), resolved);
    }

    imports
}

/// Collect all exported symbols from a module file
/// For now, we export everything at the root scope (scope 0)
#[salsa::tracked]
pub fn module_exports(db: &dyn HirTyDatabase, file: RawSourceFile) -> ModuleExports {
    let (hir_module, _parse_errors) = hir::lower_file(db, file);
    hir_module.module_exports()
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
pub fn resolve_import<'db>(
    db: &'db dyn HirTyDatabase,
    import: &hir::Import,
) -> Option<ResolvedImport> {
    let Some(module_id) = db.find_module_by_slug(&import.as_slug()) else {
        return None;
    };

    Some(ResolvedImport::Module(module_id))
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
    use alloy_workspace::WorkspaceDatabase;
    use maplit::hashmap;
    use std::collections::HashMap;

    #[test]
    fn test_cross_module_resolution() {
        let mut db = TestHirTyDatabase::default();

        // File 1: types.alloy - exports a type definition
        let types_module_id = {
            let types_content = r#"
typedef Point =
  | Point(x: Int, y: Int)
end

let origin = Point(0, 0)
"#;
            db.add_module(
                "types",
                &camino::Utf8Path::new("/test/types.alloy"),
                types_content,
            )
        };

        // File 2: main.alloy - imports from types.alloy
        let main_module_id = {
            let main_content = r#"
import types

let p = types.Point(1, 2)
"#;
            db.add_module(
                "main",
                &camino::Utf8Path::new("/test/main.alloy"),
                main_content,
            )
        };

        let resolved_imports = crate::resolve_imports(&db, main_module_id)
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expected = hashmap! {
            hir::Name::new("types") => crate::ResolvedImport::Module(types_module_id),
        };

        assert_eq!(
            resolved_imports, expected,
            "Expected exactly one import in main.alloy"
        );

        //     // Convert to ModuleId and verify it resolves to the correct file
        //     let module_id = resolved_import.to_module_id();
        //     let resolved_file = crate::resolve_module_id(&db, module_id, workspace, main_file);
        //     assert!(
        //         resolved_file.is_some(),
        //         "Expected module to resolve to a file"
        //     );
        //     assert!(
        //         resolved_file.unwrap() == types_file,
        //         "Expected module to resolve to types.alloy"
        //     );
        //
        //     // Test 3: resolve_cross_module_symbol should find Point in types.alloy
        //     let resolved_symbol = crate::resolve_cross_module_symbol(&db, types_file, &point_name);
        //     assert!(
        //         resolved_symbol.is_some(),
        //         "Expected to resolve Point symbol from types.alloy"
        //     );
        //
        //     match resolved_symbol.unwrap() {
        //         crate::ResolvedSymbol::Type { file, idx } => {
        //             assert!(
        //                 file == types_file,
        //                 "Expected symbol to come from types.alloy"
        //             );
        //             // Verify we can get the actual type definition
        //             let (types_hir, _) = hir::lower_file(&db, types_file);
        //             let _type_def = types_hir.get_type_definition(idx);
        //             // Type definition exists if we got here without panic
        //         }
        //         _ => panic!("Expected Point to resolve as a Type"),
        //     }
        //
        //     // Test 4: resolve_cross_module_symbol should find origin expression in types.alloy
        //     let resolved_symbol = crate::resolve_cross_module_symbol(&db, types_file, &origin_name);
        //     assert!(
        //         resolved_symbol.is_some(),
        //         "Expected to resolve origin symbol from types.alloy"
        //     );
        //
        //     match resolved_symbol.unwrap() {
        //         crate::ResolvedSymbol::Expression { file, .. } => {
        //             assert!(
        //                 file == types_file,
        //                 "Expected symbol to come from types.alloy"
        //             );
        //         }
        //         _ => panic!("Expected origin to resolve as an Expression"),
        //     }
        //
        //     // Note: Testing specific symbol imports (like "import types::Point") would require
        //     // creating Import instances directly, but Import::new is private.
        //     // This functionality will be tested through integration tests with actual source code.
    }
}
