#[allow(clippy::wildcard_imports)]
use super::*;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Iter;

pub type BehaviorIdx = Idx<Behavior>;

#[derive(Debug, Clone, PartialEq)]
pub struct Behavior {
    scope: ScopeIdx,
    pub attached_trait: TypeIdx,
    pub attached_type: TypeIdx,
    named_type_variables: FxHashMap<Name, TypeVariableIdx>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    values: FxHashMap<Name, ExpressionIdx>,
}

impl Behavior {
    pub fn scope(&self) -> ScopeIdx {
        self.scope
    }

    pub fn named_type_variables(&'_ self) -> Iter<'_, Name, TypeVariableIdx> {
        self.named_type_variables.iter()
    }

    pub fn type_annotations(&'_ self) -> Iter<'_, Name, TypeIdx> {
        self.type_annotations.iter()
    }

    pub fn values(&'_ self) -> Iter<'_, Name, ExpressionIdx> {
        self.values.iter()
    }
}

pub(super) fn lower_behavior(ctx: &mut LoweringCtx, ast: &ast::BehaviorDef) {
    let Some(trait_) = ast.trait_() else {
        return;
    };
    let Some(type_) = ast.type_() else {
        return;
    };

    let behavior = ctx.inside_scope("behavior", |ctx| {
        let behavior_scope = ctx.scopes.current_scope();

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
            scope: behavior_scope,
            attached_trait: trait_id,
            attached_type: type_id,
            named_type_variables,
            type_annotations,
            values,
        }
    });

    ctx.add_behavior(behavior, &ast.syntax());
}
