#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_source_file(ctx: &mut LoweringCtx, source_file: &ast::SourceFile) {
    for import in source_file.statements_by_type() {
        lower_import(ctx, &import);
    }
    for type_definition in source_file.statements_by_type() {
        lower_type_definition(ctx, &type_definition);
    }
    for trait_ in source_file.statements_by_type() {
        lower_trait(ctx, &trait_);
    }
    for behavior in source_file.statements_by_type() {
        lower_behavior(ctx, &behavior);
    }
    for type_annotation in source_file.statements_by_type() {
        lower_type_annotation(ctx, &type_annotation);
    }
    for value in source_file.statements_by_type() {
        lower_value(ctx, &value);
    }
    for module in source_file.statements_by_type() {
        lower_module(ctx, &module);
    }
    for expression in source_file.statements_by_type() {
        lower_expression(ctx, &expression);
    }
}
