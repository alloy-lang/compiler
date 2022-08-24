#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let parse = alloy_rowan_parser::parse(s);
        let syntax = parse.syntax();
        let _validation_errors = alloy_rowan_ast::validation::validate(&syntax);
        let source_file = alloy_rowan_ast::SourceFile::cast(syntax).unwrap();
        let (_database, _stmts) = alloy_rowan_hir::lower(&source_file);
    }
});
