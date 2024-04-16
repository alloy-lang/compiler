#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariable {
    Unbound,
    Constrained(Vec<TypeVariableConstraint>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariableConstraint {
    Kind(usize),
    Trait(Path),
}

pub(super) fn lower_named_type_variable(
    ctx: &mut LoweringCtx,
    ast: &ast::NamedTypeVariable,
) -> Option<(Name, TypeDefinitionIdx)> {
    let Some(name) = ast.name() else {
        // we can't add a type arg that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return None;
    };

    let constraints = lower_type_variable_constraints(ctx, &ast.constraints());
    let type_variable = if constraints.is_empty() {
        TypeVariable::Unbound
    } else {
        TypeVariable::Constrained(constraints)
    };

    Some((
        Name::new(&name),
        ctx.add_type_variable(name, type_variable, &ast.syntax()),
    ))
}

pub(super) fn lower_self_type_variable_constraints(
    ctx: &mut LoweringCtx,
    ast: &ast::SelfTypeVariable,
) -> Vec<TypeVariableConstraint> {
    lower_type_variable_constraints(ctx, &ast.constraints())
}

fn lower_type_variable_constraints(
    ctx: &mut LoweringCtx,
    type_variable_constraints: &[ast::TypeVariableConstraint],
) -> Vec<TypeVariableConstraint> {
    type_variable_constraints
        .iter()
        .map(|constraint| match constraint {
            ast::TypeVariableConstraint::TypeVariableKindConstraint(kind) => {
                TypeVariableConstraint::Kind(kind.arity())
            }
            ast::TypeVariableConstraint::TypeVariableTraitConstraint(trait_) => {
                let Some(ast_path) = trait_.trait_() else {
                    unreachable!("parsing error")
                };

                let Some(path) = ctx.resolve_reference_path(&ast_path, HirReferenceType::Type)
                else {
                    unreachable!("parsing error")
                };
                TypeVariableConstraint::Trait(path)
            }
        })
        .collect::<Vec<_>>()
}
