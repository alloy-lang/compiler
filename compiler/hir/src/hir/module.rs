#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_module(ctx: &mut LoweringCtx, module: &ast::ModuleDef) {
    for import in module.imports() {
        lower_import(ctx, &import);
    }
    for type_definition in module.type_definitions() {
        lower_type_definition(ctx, &type_definition);
    }
    for trait_ in module.traits() {
        lower_trait(ctx, &trait_);
    }
    for behavior in module.behaviors() {
        lower_behavior(ctx, &behavior);
    }
    for type_annotation in module.type_annotations() {
        lower_type_annotation(ctx, &type_annotation);
    }
    for value in module.values() {
        lower_value(ctx, &value);
    }
}
