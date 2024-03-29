#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_type_annotation(ctx: &mut LoweringCtx, ast: &ast::TypeAnnotation) {
    let Some(name) = ast.name() else {
        // todo!("validation");
        return;
    };
    let name = Name::new(name);

    let Some(type_) = ast.type_() else {
        // todo!("validation");
        return;
    };
    let type_id = lower_type_reference(ctx, &type_);
    ctx.add_type_annotation(name, type_id, &ast.syntax());
}
