#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_type_annotation(ctx: &mut LoweringCtx, ast: &ast::TypeAnnotation) {
    let Some(name) = ast.name() else {
        // todo!("validation");
        return;
    };
    let name = Name::new(name);

    let type_id = ctx.inside_scope("type annotation", |ctx| {
        for type_arg in ast.named_type_variables() {
            let Some(name) = type_arg.name() else {
                // todo!("validation");
                continue;
            };
            ctx.add_type_variable(name, &type_arg.syntax());
        }

        let Some(type_) = ast.type_() else {
            // todo!("validation");
            return None;
        };
        Some(lower_type_reference(ctx, &type_))
    });

    let Some(type_id) = type_id else {
        // todo!("validation");
        return;
    };

    ctx.add_type_annotation(name, type_id, &ast.syntax());
}
