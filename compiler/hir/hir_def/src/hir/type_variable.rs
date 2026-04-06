#[allow(clippy::wildcard_imports)]
use super::*;

pub type TypeVariableIdx = Idx<TypeVariable>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    pub name: Name,
    pub kind: TypeVariableKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariableKind {
    Unbound,
    Constrained(NonEmpty<TypeVariableConstraintIdx>),
}

pub type TypeVariableConstraintIdx = Idx<TypeVariableConstraint>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariableConstraint {
    Kind(usize),
    Trait(TypeIdx),
}

pub(super) fn lower_named_type_variable(
    ctx: &mut LoweringCtx,
    ast: &ast::NamedTypeVariable,
) -> Option<(Name, TypeVariableIdx)> {
    let Some(name) = ast.name() else {
        // we can't add a type arg that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return None;
    };

    let constraints = lower_type_variable_constraints(ctx, &ast.constraints());
    let type_variable = if constraints.is_empty() {
        TypeVariableKind::Unbound
    } else {
        TypeVariableKind::Constrained(unsafe { NonEmpty::new_unchecked(constraints) })
    };

    Some((
        Name::new(name.text()),
        ctx.add_type_variable(name.text(), type_variable, ast),
    ))
}

pub(super) fn lower_self_type_variable_constraints(
    ctx: &mut LoweringCtx,
    ast: &ast::SelfTypeVariable,
) -> Vec<TypeVariableConstraintIdx> {
    lower_type_variable_constraints(ctx, &ast.constraints())
}

fn lower_type_variable_constraints(
    ctx: &mut LoweringCtx,
    type_variable_constraints: &[ast::TypeVariableConstraint],
) -> Vec<TypeVariableConstraintIdx> {
    type_variable_constraints
        .iter()
        .map(|ast| {
            let constraint = match ast {
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
                    let type_reference =
                        ctx.add_type_reference(TypeReference::Named(path.clone()), &ast_path);
                    TypeVariableConstraint::Trait(type_reference)
                }
            };

            ctx.add_type_variable_constraint(constraint, ast)
        })
        .collect::<Vec<_>>()
}
