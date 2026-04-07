#[allow(clippy::wildcard_imports)]
use super::*;
use rustc_hash::FxHashMap;

pub type TraitIdx = Idx<Trait>;

#[derive(Debug, Clone, PartialEq)]
pub enum TraitMember {
    /// A member with a type annotation but no implementation (abstract)
    Abstract {
        name: Name,
        type_annotation: TypeIdx,
    },
    /// A member with an implementation (may also have a type annotation)
    Concrete {
        name: Name,
        type_annotation: Option<TypeIdx>,
        value: ExpressionIdx,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub(crate) name: Name,
    /// The scope where this trait's members are defined
    scope: ScopeIdx,
    self_constraints: Vec<TypeVariableConstraintIdx>,
    named_type_variables: FxHashMap<Name, TypeVariableIdx>,
    members: Vec<TraitMember>,
}

pub(super) fn lower_trait(ctx: &mut LoweringCtx, ast: &ast::TraitDef) {
    let Some(name_ast) = ast.name() else {
        // we can't lower a trait that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let name = Name::new(name_ast.text());

    let trait_ = ctx.inside_scope("trait", |ctx| {
        // Capture the current scope (the trait's scope)
        let trait_scope = ctx.scopes.current_scope();

        let named_type_variables = ast
            .named_type_variables()
            .iter()
            .filter_map(|type_var| lower_named_type_variable(ctx, type_var))
            .collect();
        let self_constraints = {
            let mut self_constraints = match ast.self_type_variables().as_slice() {
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
            let self_constraint = ctx.add_type_variable_constraint(
                TypeVariableConstraint::SelfRef(trait_scope),
                &name_ast,
            );
            self_constraints.push(self_constraint);

            self_constraints
        };

        let type_annotations: FxHashMap<Name, TypeIdx> = ast
            .type_annotations()
            .iter()
            .filter_map(|type_annotation| lower_type_annotation(ctx, type_annotation))
            .collect();

        let values: FxHashMap<Name, ExpressionIdx> = ast
            .values()
            .iter()
            .filter_map(|value| lower_value(ctx, value))
            .collect();

        // Build members from type_annotations and values
        let mut members = Vec::new();
        let mut processed_names = HashSet::new();

        // Process type annotations
        for (name, type_idx) in type_annotations {
            processed_names.insert(name.clone());
            if let Some(value_idx) = values.get(&name) {
                // Has both type annotation and value -> Concrete with type
                members.push(TraitMember::Concrete {
                    name,
                    type_annotation: Some(type_idx),
                    value: *value_idx,
                });
            } else {
                // Has type annotation but no value -> Abstract
                members.push(TraitMember::Abstract {
                    name,
                    type_annotation: type_idx,
                });
            }
        }

        // Process values that don't have type annotations
        for (name, value_idx) in values {
            if !processed_names.contains(&name) {
                // Has value but no type annotation -> Concrete without type
                members.push(TraitMember::Concrete {
                    name,
                    type_annotation: None,
                    value: value_idx,
                });
            }
        }

        Trait {
            name: name.clone(),
            scope: trait_scope,
            self_constraints,
            named_type_variables,
            members,
        }
    });

    ctx.add_trait(trait_, ast);
}

impl Trait {
    /// Get all members of this trait
    pub fn members(&self) -> &[TraitMember] {
        &self.members
    }

    /// Get the name of this trait
    pub fn name(&self) -> &Name {
        &self.name
    }

    /// Get the scope where this trait's members are defined
    pub fn scope(&self) -> ScopeIdx {
        self.scope
    }

    /// Get the self-type constraints for this trait (e.g., `self = #Type[_] + Eq`)
    pub fn self_constraints(&self) -> &[TypeVariableConstraintIdx] {
        &self.self_constraints
    }

    /// Get all abstract members (those with type annotations but no implementations)
    pub fn abstract_members(&self) -> impl Iterator<Item = (&Name, TypeIdx)> {
        self.members.iter().filter_map(|member| match member {
            TraitMember::Abstract {
                name,
                type_annotation,
            } => Some((name, *type_annotation)),
            TraitMember::Concrete { .. } => None,
        })
    }

    /// Get all abstract members (those with type annotations but no implementations)
    pub fn abstract_member(&self, name: &Name) -> Option<(&Name, TypeIdx)> {
        self.abstract_members()
            .find(move |(member_name, _)| member_name == &name)
    }

    /// Get all concrete members (those with implementations)
    pub fn concrete_members(&self) -> impl Iterator<Item = (&Name, ExpressionIdx)> {
        self.members.iter().filter_map(|member| match member {
            TraitMember::Concrete { name, value, .. } => Some((name, *value)),
            TraitMember::Abstract { .. } => None,
        })
    }
}
