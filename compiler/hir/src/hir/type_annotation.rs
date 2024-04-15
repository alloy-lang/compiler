#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_type_annotation(ctx: &mut LoweringCtx, ast: &ast::TypeAnnotation) {
    let type_id = ctx.inside_scope("type annotation", |ctx| {
        for type_arg in ast.named_type_variables() {
            let Some(name) = type_arg.name() else {
                // we can't add a type arg that we don't have a name for
                // we can skip it since it'll be reported as a parsing error
                continue;
            };
            ctx.add_type_variable(name, &type_arg.syntax());
        }

        match ast.type_() {
            None => ctx.add_missing_type_reference(&ast.syntax()),
            Some(type_) => lower_type_reference(ctx, &type_),
        }
    });

    let Some(name) = ast.name() else {
        // we can't lower a type annotation that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let name = Name::new(name);

    ctx.add_type_annotation(name, type_id, &ast.syntax());
}
