#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, PartialEq)]
pub enum BuiltInType {}

pub type TypeIdx = Idx<Type>;

#[derive(Debug, PartialEq)]
pub enum Type {
    Missing,
    SelfRef,
    Unit,
    Unknown,
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

pub(super) fn lower_type(ctx: &mut LoweringCtx, ast: &ast::Type) -> TypeIdx {
    let type_ = lower_type_inner(ctx, ast);
    ctx.add_type_(type_, &ast.syntax())
}

fn lower_type_inner(ctx: &mut LoweringCtx, ast: &ast::Type) -> Type {
    match ast {
        ast::Type::SelfType(_) => Type::SelfRef,
        ast::Type::UnitType(_) => Type::Unit,
        ast::Type::NilIdentifier(_) => Type::Unknown,
        ast::Type::TypeIdentifier(t) => {
            let Some(path) = t.name() else {
                // todo!("validation");
                return Type::Missing;
            };

            let Ok(name) = Path::try_from(path.segments()) else {
                // todo!("validation");
                return Type::Missing;
            };

            Type::Named(name)
        }
        ast::Type::LambdaType(t) => {
            let Some(arg_type) = t.arg_type() else {
                // todo!("validation");
                return Type::Missing;
            };
            let arg_type = lower_type(ctx, &arg_type);

            let Some(return_type) = t.return_type() else {
                // todo!("validation");
                return Type::Missing;
            };
            let return_type = lower_type(ctx, &return_type);

            Type::Lambda {
                arg_type,
                return_type,
            }
        }
        ast::Type::TupleType(t) => {
            let types = t
                .members()
                .iter()
                .map(|member| lower_type(ctx, &member))
                .collect::<Vec<_>>();
            Type::Tuple(types)
        }
        ast::Type::ParenthesizedType(t) => {
            let Some(inner) = t.inner() else {
                // todo!("validation");
                return Type::Missing;
            };

            let inner = lower_type(ctx, &inner);
            Type::ParenthesizedType(inner)
        }
        ast::Type::BoundedType(t) => {
            let Some(base) = t.base() else {
                // todo!("validation");
                return Type::Missing;
            };
            let base = lower_type(ctx, &base);

            let args = t
                .args()
                .iter()
                .map(|member| lower_type(ctx, &member))
                .collect::<Vec<_>>();

            Type::Bounded { base, args }
        }
    }
}
