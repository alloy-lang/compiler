#[allow(clippy::wildcard_imports)]
use super::*;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BuiltInType {
    Int,
    Fraction,
    String,
    Char,
    Bool,
}

impl TryFrom<ast::Path> for BuiltInType {
    type Error = ();

    fn try_from(path: ast::Path) -> Result<Self, Self::Error> {
        match path.segments().join("::").as_str() {
            "Int" => Ok(Self::Int),
            "Fraction" => Ok(Self::Fraction),
            "String" => Ok(Self::String),
            "Char" => Ok(Self::Char),
            "Bool" => Ok(Self::Bool),
            _ => Err(()),
        }
    }
}

pub type TypeIdx = Idx<TypeReference>;

#[derive(Debug, PartialEq)]
pub enum TypeReference {
    Unknown,
    Missing,
    SelfRef,
    Unit,
    Named(Path),
    BuiltIn(BuiltInType),
    Lambda {
        arg_type: TypeIdx,
        return_type: TypeIdx,
    },
    Tuple(Vec<TypeIdx>),
    ParenthesizedType(TypeIdx),
    Bounded {
        base: TypeIdx,
        args: Vec<TypeIdx>,
    },
}

pub(super) fn lower_type_reference(ctx: &mut LoweringCtx, ast: &ast::Type) -> TypeIdx {
    let type_ = lower_type_inner(ctx, ast);
    ctx.add_type_reference(type_, &ast.syntax())
}

fn lower_type_inner(ctx: &mut LoweringCtx, ast: &ast::Type) -> TypeReference {
    match ast {
        ast::Type::SelfType(_) => TypeReference::SelfRef,
        ast::Type::UnitType(_) => TypeReference::Unit,
        ast::Type::NilIdentifier(_) => TypeReference::Unknown,
        ast::Type::TypeIdentifier(t) => {
            let Some(path) = t.name() else {
                unreachable!("parsing error")
            };

            BuiltInType::try_from(path.clone()).map_or_else(
                |_| {
                    let Some(name) = ctx.resolve_reference_path(&path, HirReferenceType::Type)
                    else {
                        unreachable!("parsing error")
                    };

                    TypeReference::Named(name)
                },
                TypeReference::BuiltIn,
            )
        }
        ast::Type::LambdaType(t) => {
            let Some(arg_type) = t.arg_type() else {
                unreachable!("parsing error")
            };
            let arg_type = lower_type_reference(ctx, &arg_type);

            let Some(return_type) = t.return_type() else {
                unreachable!("parsing error")
            };
            let return_type = lower_type_reference(ctx, &return_type);

            TypeReference::Lambda {
                arg_type,
                return_type,
            }
        }
        ast::Type::TupleType(t) => {
            let types = t
                .members()
                .iter()
                .map(|member| lower_type_reference(ctx, member))
                .collect::<Vec<_>>();
            TypeReference::Tuple(types)
        }
        ast::Type::ParenthesizedType(t) => {
            let Some(inner) = t.inner() else {
                unreachable!("parsing error")
            };

            let inner = lower_type_reference(ctx, &inner);
            TypeReference::ParenthesizedType(inner)
        }
        ast::Type::BoundedType(t) => {
            let Some(base) = t.base() else {
                unreachable!("parsing error")
            };
            let base = lower_type_reference(ctx, &base);

            let args = t
                .args()
                .iter()
                .map(|member| lower_type_reference(ctx, member))
                .collect::<Vec<_>>();

            TypeReference::Bounded { base, args }
        }
    }
}
