#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_statement(ctx: &mut LoweringCtx, statement: &ast::Statement) {
    match statement {
        ast::Statement::Expression(e) => {
            lower_expression(ctx, e);
        }
        ast::Statement::ModuleDef(m) => {
            lower_module(ctx, m);
        }
        ast::Statement::ImportDef(i) => lower_import(ctx, i),
        ast::Statement::TraitDef(t) => lower_trait(ctx, t),
        ast::Statement::BehaviorDef(b) => lower_behavior(ctx, b),
        ast::Statement::TypeDefinition(td) => lower_type_definition(ctx, td),
        ast::Statement::TypeAnnotation(ta) => {
            lower_type_annotation(ctx, ta);
        }
        ast::Statement::ValueDef(v) => {
            lower_value(ctx, v);
        }
    }
}
