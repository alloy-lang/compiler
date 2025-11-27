#[allow(clippy::wildcard_imports)]
use super::*;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Behavior {
    pub attached_trait: TypeIdx,
    pub attached_type: TypeIdx,
    named_type_variables: FxHashMap<Name, TypeDefinitionIdx>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    values: FxHashMap<Name, ExpressionIdx>,
}

pub(super) fn lower_behavior(ctx: &mut LoweringCtx, ast: &ast::BehaviorDef) {
    let Some(trait_) = ast.trait_() else {
        return;
    };
    let Some(type_) = ast.type_() else {
        return;
    };

    let behavior = ctx.inside_scope("behavior", |ctx| {
        let named_type_variables = ast
            .named_type_variables()
            .iter()
            .filter_map(|type_var| lower_named_type_variable(ctx, type_var))
            .collect();
        let trait_id = lower_type_reference(ctx, &trait_);
        let type_id = lower_type_reference(ctx, &type_);

        let type_annotations = ast
            .type_annotations()
            .iter()
            .filter_map(|type_annotation| lower_type_annotation(ctx, type_annotation))
            .collect();

        let values = ast
            .values()
            .iter()
            .filter_map(|value| lower_value(ctx, value))
            .collect();

        Behavior {
            attached_trait: trait_id,
            attached_type: type_id,
            named_type_variables,
            type_annotations,
            values,
        }
    });

    ctx.add_behavior(behavior, &ast.syntax());
}
