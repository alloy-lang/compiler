#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_type_annotation(
    ctx: &mut LoweringCtx,
    ast: &ast::TypeAnnotation,
) -> Option<(Name, TypeIdx)> {
    let type_id = ctx.inside_scope("type annotation", |ctx| {
        for type_var in ast.named_type_variables() {
            lower_named_type_variable(ctx, &type_var);
        }

        match ast.type_() {
            None => ctx.add_missing_type_reference(&ast.syntax()),
            Some(type_) => lower_type_reference(ctx, &type_),
        }
    });

    let Some(name) = ast.name() else {
        // we can't lower a type annotation that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return None;
    };
    let name = Name::new(name.text());

    Some((
        name.clone(),
        ctx.add_type_annotation(name, type_id, &ast.syntax()),
    ))
}
