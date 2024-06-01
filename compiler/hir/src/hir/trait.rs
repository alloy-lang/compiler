#[allow(clippy::wildcard_imports)]
use super::*;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    name: Name,
    self_constraints: Vec<TypeVariableConstraint>,
    named_type_variables: FxHashMap<Name, TypeDefinitionIdx>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    values: FxHashMap<Name, ExpressionIdx>,
}

pub(super) fn lower_trait(ctx: &mut LoweringCtx, ast: &ast::TraitDef) {
    let Some(name) = ast.name() else {
        // we can't lower a trait that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let name = Name::new(name.text());

    let trait_ = ctx.inside_scope("trait", |ctx| {
        let named_type_variables = ast
            .named_type_variables()
            .iter()
            .filter_map(|type_var| lower_named_type_variable(ctx, type_var))
            .collect();

        let self_constraints = match ast.self_type_variables().as_slice() {
            [] => Vec::new(),
            [type_var] => lower_self_type_variable_constraints(ctx, type_var),
            many => {
                ctx.error(
                    LoweringErrorKind::MultipleSelfTypeVariablesInTraitDefinition {
                        trait_name: name.clone(),
                        ranges: many.iter().map(AstElement::range).collect(),
                    },
                    ast.range(),
                );
                Vec::new()
            }
        };

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

        Trait {
            name: name.clone(),
            self_constraints,
            named_type_variables,
            type_annotations,
            values,
        }
    });

    ctx.add_trait(name, trait_, &ast.syntax());
}
