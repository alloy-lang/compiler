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

pub(super) fn lower_type(ctx: &mut LoweringCtx, ast: &ast::Type) {
    let ty = match ast {
        ast::Type::SelfType(_) => {}
        ast::Type::UnitType(_) => {}
        ast::Type::NilIdentifier(_) => todo!(),
        ast::Type::TypeIdentifier(_) => {}
        ast::Type::LambdaType(_) => {}
        ast::Type::TupleType(_) => {}
        ast::Type::ParenthesizedType(_) => {}
        ast::Type::BoundedType(_) => {}
    };
}
