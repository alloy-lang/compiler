#[allow(clippy::wildcard_imports)]
use super::*;

pub type TypeDefinitionIdx = Idx<TypeDefinition>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub(crate) name: Name,
    pub(crate) kind: TypeDefinitionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinitionKind {
    Missing,
    TypeVariable(TypeVariable),
    Single(TypeDefinitionMember),
    Union(Vec<TypeDefinitionMember>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinitionMember {
    name: Name,
    properties: Vec<TypeIdx>,
}

pub(super) fn lower_type_definition(ctx: &mut LoweringCtx, ast: &ast::TypeDefinition) {
    let Some(parent_name) = ast.name() else {
        // we can't lower a type that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let parent_name = Name::new(parent_name);

    let type_definition = ctx.inside_scope("type definition", |ctx| {
        let type_args = ast.type_args();
        for type_arg in type_args {
            let name = type_arg.text();
            ctx.add_type_variable(name, TypeVariable::Unbound, &type_arg.syntax());
        }

        let mut members = vec![];
        for member in ast.types() {
            let Some(sub_name) = member.name() else {
                // if the sub_name is missing for a type, we can skip it since it'll be reported as a parsing error
                continue;
            };

            let properties = member
                .properties()
                .iter()
                .map(|property| lower_type_reference(ctx, property))
                .collect::<Vec<_>>();

            members.push(TypeDefinitionMember {
                name: Name::new(sub_name),
                properties,
            });
        }

        let kind = match &members[..] {
            [] => TypeDefinitionKind::Missing,
            [_] => TypeDefinitionKind::Single(first(&mut members)),
            _ => TypeDefinitionKind::Union(members),
        };

        TypeDefinition {
            name: parent_name,
            kind,
        }
    });

    ctx.add_type_definition(type_definition, &ast.syntax());
}

fn first<T>(v: &mut Vec<T>) -> T {
    v.drain(..).next().expect("")
}
