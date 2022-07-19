#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let parse = alloy_rowan_parser::parse(s);
        let root = alloy_rowan_ast::Root::cast(parse.syntax()).unwrap();
        let (_database, _stmts) = alloy_rowan_hir::lower(root);
    }
});
