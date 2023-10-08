#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_source_file(ctx: &mut LoweringCtx, source_file: &ast::SourceFile) {
    if let Some(module) = source_file.module() {
        lower_module(ctx, &module);
    }
}

pub(super) fn lower_repl_line(ctx: &mut LoweringCtx, source_file: &ast::SourceFile) {
    for statement in source_file.statements() {
        lower_statement(ctx, &statement);
    }
}
