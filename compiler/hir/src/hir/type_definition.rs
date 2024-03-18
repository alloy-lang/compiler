#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, PartialEq)]
pub struct TypeDefinition {
    pub(crate) name: Name,
    pub(crate) kind: TypeDefinitionKind,
}

#[derive(Debug, PartialEq)]
pub enum TypeDefinitionKind {
    Missing,
    Single(TypeDefinitionMember),
    Union(Vec<TypeDefinitionMember>),
}

#[derive(Debug, PartialEq)]
pub struct TypeDefinitionMember {
    name: Name,
    properties: Vec<TypeIdx>,
}

pub(super) fn lower_type_definition(ctx: &mut LoweringCtx, ast: &ast::TypeDefinition) {
    let Some(parent_name) = ast.name() else {
        // unreachable!("parsing error")
        return;
    };
    let parent_name = Name::new(parent_name);

    let type_definition = ctx.inside_scope("type definition", |ctx| {
        let mut members = vec![];
        for member in ast.types() {
            let Some(sub_name) = member.name() else {
                // unreachable!("parsing error");
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
