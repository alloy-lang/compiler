#[allow(clippy::wildcard_imports)]
use super::*;

pub type TypeDefinitionIdx = Idx<TypeDefinition>;

#[derive(Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: Name,
    pub type_args: Vec<TypeVariableIdx>,
    pub kind: TypeDefinitionKind,
}

impl fmt::Debug for TypeDefinition {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut debug_struct = f.debug_struct("TypeDefinition");
        debug_struct.field("name", &self.name);
        if !self.type_args.is_empty() {
            debug_struct.field("type_args", &self.type_args);
        }
        debug_struct.field("kind", &self.kind);

        debug_struct.finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinitionKind {
    Missing,
    Single(TypeDefinitionMember),
    Union(Vec<TypeDefinitionMember>),
}

impl TypeDefinitionKind {
    /// Check if this type definition has a variant with the given name
    #[must_use]
    pub fn has_variant(&self, variant_name: &Name) -> bool {
        match self {
            TypeDefinitionKind::Single(member) => member.name() == variant_name,
            TypeDefinitionKind::Union(members) => members.iter().any(|m| m.name() == variant_name),
            TypeDefinitionKind::Missing => false,
        }
    }
    #[must_use]
    pub fn has_variants(&self) -> bool {
        match self {
            TypeDefinitionKind::Union(_) => true,
            TypeDefinitionKind::Single(_) | TypeDefinitionKind::Missing => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinitionMember {
    name: Name,
    properties: Vec<TypeIdx>,
}

impl TypeDefinitionMember {
    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn properties(&self) -> &[TypeIdx] {
        &self.properties
    }
}

pub(super) fn lower_type_definition(ctx: &mut LoweringCtx, ast: &ast::TypeDefinition) {
    let Some(parent_name) = ast.name() else {
        // we can't lower a type that we don't have a name for
        // we can skip it since it'll be reported as a parsing error
        return;
    };
    let parent_name = Name::new(parent_name.text());

    let type_definition = ctx.inside_scope("type definition", |ctx| {
        let type_args = ast.type_args();

        let type_args = type_args
            .iter()
            .map(|type_arg| {
                let name = type_arg.text();
                ctx.add_type_variable(name, TypeVariableKind::Unbound, &type_arg.syntax())
            })
            .collect::<Vec<_>>();

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
                name: Name::new(sub_name.text()),
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
            type_args,
            kind,
        }
    });

    ctx.add_type_definition(type_definition, &ast.syntax());
}

fn first<T>(v: &mut Vec<T>) -> T {
    v.drain(..).next().expect("")
}
