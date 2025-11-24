mod db;

pub mod parser {
    pub use alloy_parser::parse_source_file;
}

use alloy_project::Project;
use alloy_workspace::{ModuleId, SourceFile, WorkspaceDatabase};
use std::collections::HashMap;
// compile
// input:
// - external packages: Vec<Package> (these are the packages that the current package depends on, maybe be incomplete which results in a compilation error)
// - current package: Package (this package we are working on and contains the entrypoint/main function)
// - output: Utf8Path (the path to the output file that will be generated after compilation)
// output:
// - Vec<u8>

pub struct CompilationTarget {
    _entrypoint: Option<ModuleId>,
    _type: CompilationTargetType,
}
pub enum CompilationTargetType {
    Lib,
    Bin,
    Test,
}

pub struct PackageMetadata {
    name: String,
}

pub fn compile<'db>(
    db: &'db mut db::CompilerDatabase,
    _external_packages: &[PackageMetadata],
    current_project: Project,
    _target: CompilationTarget,
) {
    let module_ids: Vec<ModuleId> = current_project
        .modules()
        .map(|file| db.add_module(file.slug(), file.path(), file.contents()))
        .collect();

    // Phase 1 & 2: Parse and lower all files to HIR (single pass)
    // This efficiently parses each file once and returns both parse errors and HIR
    let mut parse_errors = HashMap::new();
    let mut hir_modules = HashMap::new();
    let mut lowering_errors = HashMap::new();
    let mut lowering_warnings = HashMap::new();

    for module_id in module_ids.iter() {
        let source_file = db.get_source(*module_id);
        let source_file = match source_file {
            SourceFile::Raw(raw) => raw,
            SourceFile::Virtual(_) => continue,
        };

        let (hir_module, parse_errs) = alloy_hir::lower_file(db, *source_file);

        // Collect parse errors
        if !parse_errs.is_empty() {
            parse_errors.insert(module_id, parse_errs);
        }

        // Collect lowering errors and warnings
        if !hir_module.errors().is_empty() {
            lowering_errors.insert(module_id, hir_module.errors().to_vec());
        }
        if !hir_module.warnings().is_empty() {
            lowering_warnings.insert(module_id, hir_module.warnings().to_vec());
        }

        hir_modules.insert(module_id, hir_module);
    }

    // TODO: Phase 3: Type checking
    // TODO: Phase 4: IR generation (only for files reachable from entrypoint)

    // entrypoint(s) of the current package
    //   for a binary, this is the `main` function in the entrypoint file
    //   for a library, these would be the public members in the entrypoint file (`lib.alloy`)

    // strategies:
    // 1. "on-demand" file processing
    // create a full map of module name to source file
    // starting with the entrypoint of the current package: parse -> AST -> HIR -> name resolution -> type checking -> IR generation
    // during name resolution
    //   if we encounter a module we haven't processed yet
    //     look up the module based on the path
    //       if we can't find a file for that module, mark that import as "unknown module reference"
    //       if we can find a file for that module, pause the current module's name resolution and start processing that file
    //         if that completes successfully, search the module's exported members for the one we need
    //           if we can't find one, mark the reference as "unknown <kind> reference"
    //
    // pros:
    // - no wasted time processing files that don't end of getting used
    // - probably a simpler model in general (maybe?)
    // - cacheable (maybe?)
    // cons:
    // - non-parallelizable
    // - "small" changes to the entrypoint (ie. adding an import for a module) could cause significant increases to compile time
    // - files that don't get processed won't have any warnings/errors emitted
    // - LSP requires being able to support files that aren't yet referenced from other parts of the code yet (LSP might be better to think about separately)
    //

    // 2. "initial" processing on every file, remainder "on-demand" (query)
    // create a full map of module name to source file
    // parallelizable "parse -> AST" for each file

    // 3. "passes" on each file
    // pros:
    // cons:
    // - name resolution requires files being processed in a specific order
    // - incremental compilation becomes tricky

    // for each package
    // for each file in the package
    //   parse
    //   collect parsing errors for the file
    //   parse tree -> AST

    // type checking should report errors for the entire codebase, even if it's not referenced by the entrypoint
    // IR generation shouldn't generate for anything not referenced by the entrypoints (defined above)
}
