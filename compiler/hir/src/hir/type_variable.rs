#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, PartialEq)]
pub enum TypeVariable {
    Unbound,
    Constrained(Vec<TypeVariableConstraint>),
}

#[derive(Debug, PartialEq)]
pub enum TypeVariableConstraint {
    Kind(usize),
    Trait(Path),
}

pub(super) fn lower_named_type_variable(ctx: &mut LoweringCtx, ast: &ast::NamedTypeVariable) {
    let Some(name) = ast.name() else {
        // we can't add a type arg that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };

    let constraints = ast
        .constraints()
        .iter()
        .map(|constraint| match constraint {
            ast::TypeVariableConstraint::TypeVariableKindConstraint(kind) => {
                TypeVariableConstraint::Kind(kind.arity())
            }
            ast::TypeVariableConstraint::TypeVariableTraitConstraint(trait_) => {
                let Some(ast_path) = trait_.trait_() else {
                    unreachable!("parsing error")
                };

                let Some(path) = ctx.resolve_reference_path(&ast_path) else {
                    unreachable!("parsing error")
                };
                TypeVariableConstraint::Trait(path)
            }
        })
        .collect::<Vec<_>>();

    let type_variable = if constraints.is_empty() {
        TypeVariable::Unbound
    } else {
        TypeVariable::Constrained(constraints)
    };

    ctx.add_type_variable(name, type_variable, &ast.syntax());
}
