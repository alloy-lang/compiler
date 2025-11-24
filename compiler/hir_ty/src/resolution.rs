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
        symbol: ResolvedSymbol,
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
fn resolve_import<'db>(db: &'db dyn HirTyDatabase, import: &hir::Import) -> Option<ResolvedImport> {
    if let Some(module_id) = db.find_module_by_slug(&import.as_slug()) {
        return Some(ResolvedImport::Module(module_id));
    };

    if let Some(imported_module_id) = db.find_module_by_slug(&import.as_slug_no_last()) {
        return resolve_cross_module_symbol(db, imported_module_id, import.last()).map(|symbol| {
            ResolvedImport::Symbol {
                module_id: imported_module_id,
                symbol_name: import.last().clone(),
                symbol,
            }
        });
    };

    None
}

/// Resolve a cross-module reference
/// Given an import and a symbol name, find the symbol in the imported module
pub fn resolve_cross_module_symbol(
    db: &dyn HirTyDatabase,
    imported_module_id: ModuleId,
    symbol_name: &hir::Name,
) -> Option<ResolvedSymbol> {
    let imported_file = db.get_source(imported_module_id);
    let imported_file = match imported_file {
        SourceFile::Raw(raw) => raw,
        SourceFile::Virtual(_) => todo!("need to implement virtual module support"),
    };

    let (hir_module, _) = hir::lower_file(db, *imported_file);
    let exports = hir_module.module_exports();

    // Check if it's a type
    if let Some(type_idx) = exports.types.get(symbol_name) {
        return Some(ResolvedSymbol::Type {
            module: imported_module_id,
            idx: *type_idx,
        });
    }

    // Check if it's a trait
    if let Some(trait_idx) = exports.traits.get(symbol_name) {
        return Some(ResolvedSymbol::Trait {
            module: imported_module_id,
            idx: *trait_idx,
        });
    }

    // Check if it's an expression
    if let Some(expr_idx) = exports.expressions.get(symbol_name) {
        return Some(ResolvedSymbol::Expression {
            module: imported_module_id,
            idx: *expr_idx,
        });
    }

    None
}

/// Represents a resolved symbol that may be in any module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedSymbol {
    Type {
        module: ModuleId,
        idx: hir::TypeDefinitionIdx,
    },
    Trait {
        module: ModuleId,
        idx: hir::TraitIdx,
    },
    Expression {
        module: ModuleId,
        idx: hir::ExpressionIdx,
    },
}

#[cfg(test)]
mod tests {
    use crate::tests::TestHirTyDatabase;
    use alloy_hir as hir;
    use alloy_workspace::{ModuleId, WorkspaceDatabase};
    use la_arena::RawIdx;
    use maplit::hashmap;
    use std::collections::HashMap;

    fn add_test_module(db: &mut TestHirTyDatabase, slug: &str, contents: &str) -> ModuleId {
        db.add_module(slug, &camino::Utf8Path::new("/test/stuff.alloy"), contents)
    }

    fn add_types_module(db: &mut TestHirTyDatabase) -> ModuleId {
        add_test_module(
            db,
            "types",
            r#"
                trait TestTrait1 where
                    typeof test : Int

                    let thing = test
                end

                typedef Point =
                    | Point(x: Int, y: Int)
                end

                let origin = Point(0, 0)
                "#,
        )
    }

    #[test]
    fn test_module_import_resolution() {
        let mut db = TestHirTyDatabase::default();

        let types_module_id = add_types_module(&mut db);
        let main_module_id = add_test_module(
            &mut db,
            "main",
            r#"
                import types

                let p = Point(1, 2)
                "#,
        );

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
    }

    #[test]
    fn test_module_type_resolution() {
        let mut db = TestHirTyDatabase::default();

        let types_module_id = add_types_module(&mut db);
        let main_module_id = add_test_module(
            &mut db,
            "main",
            r#"
                import types::Point

                let p = Point(1, 2)
                "#,
        );

        let resolved_imports = crate::resolve_imports(&db, main_module_id)
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expected = hashmap! {
            hir::Name::new("Point") => crate::ResolvedImport::Symbol {
                module_id: types_module_id,
                symbol_name: hir::Name::new("Point"),
                symbol: crate::ResolvedSymbol::Type {
                    module: types_module_id,
                    idx: hir::TypeDefinitionIdx::from_raw(RawIdx::from_u32(0)),
                }
            },
        };

        assert_eq!(
            resolved_imports, expected,
            "Expected exactly one import in main.alloy"
        );
    }

    #[test]
    fn test_module_value_resolution() {
        let mut db = TestHirTyDatabase::default();

        let types_module_id = add_types_module(&mut db);
        let main_module_id = add_test_module(
            &mut db,
            "main",
            r#"
                import types::origin

                let p = origin
                "#,
        );

        let resolved_imports = crate::resolve_imports(&db, main_module_id)
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expected = hashmap! {
            hir::Name::new("origin") => crate::ResolvedImport::Symbol {
                module_id: types_module_id,
                symbol_name: hir::Name::new("origin"),
                symbol: crate::ResolvedSymbol::Expression {
                    module: types_module_id,
                    idx: hir::ExpressionIdx::from_raw(RawIdx::from_u32(3)),
                }
            },
        };

        assert_eq!(
            resolved_imports, expected,
            "Expected exactly one import in main.alloy"
        );
    }

    #[test]
    fn test_module_trait_resolution() {
        let mut db = TestHirTyDatabase::default();

        let types_module_id = add_types_module(&mut db);
        let main_module_id = add_test_module(
            &mut db,
            "main",
            r#"
                import types::TestTrait1
                "#,
        );

        let resolved_imports = crate::resolve_imports(&db, main_module_id)
            .into_iter()
            .collect::<HashMap<_, _>>();
        let expected = hashmap! {
            hir::Name::new("TestTrait1") => crate::ResolvedImport::Symbol {
                module_id: types_module_id,
                symbol_name: hir::Name::new("TestTrait1"),
                symbol: crate::ResolvedSymbol::Trait {
                    module: types_module_id,
                    idx: hir::TraitIdx::from_raw(RawIdx::from_u32(0)),
                }
            },
        };

        assert_eq!(
            resolved_imports, expected,
            "Expected exactly one import in main.alloy"
        );
    }
}
