use itertools::Itertools;
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Module {
    name: String,
    imports: Vec<Import>,
    typedefs: Vec<Typedef>,
    traits: Vec<Trait>,
    behavior: Vec<Behavior>,
    values: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Import {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Typedef {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Trait {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Behavior {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Value {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Type {
    Unit,
    Identifier(String),
    Variable(String),
    Lambda {
        arg_type: Box<Type>,
        return_type: Box<Type>,
    },
    Tuple(NonEmpty<Type>),
}

impl Type {
    // pub fn union(types: Vec<Type>) -> Type {
    //     match &types[..] {
    //         [] => Type::Unit,
    //         [arg] => arg.clone(),
    //         _ => unsafe {
    //             Type::Union {
    //                 types: Type::Tuple(NonEmpty::new_unchecked(types))
    //             }
    //         },
    //     }
    // }

    #[must_use]
    pub fn tuple(types: Vec<Type>) -> Type {
        match &types[..] {
            [] => Type::Unit,
            [arg] => arg.clone(),
            _ => unsafe {
                Type::Tuple(NonEmpty::new_unchecked(types))
            },
        }
    }

    #[must_use]
    pub fn lambda<T1, T2>(arg_type: T1, return_type: T2) -> Type
    where
        T1: Into<Box<Type>>,
        T2: Into<Box<Type>>,
    {
        Type::Lambda {
            arg_type: arg_type.into(),
            return_type: return_type.into(),
        }
    }

    #[must_use]
    pub fn identifier<S>(s: S) -> Type
    where
        S: Into<String>,
    {
        Type::Identifier(s.into())
    }

    #[must_use]
    pub fn variable<S>(id: S) -> Type
    where
        S: Into<String>,
    {
        Type::Variable(id.into())
    }

    // pub fn bound<T>(t: T, binds: Vec<Type>) -> Type
    //     where
    //         T: Into<Box<Type>>,
    // {
    //     Type::Bound { t: t.into(), binds }
    // }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Expr {
    Unit,
    Identifier(String),
    Apply(Box<Expr>, Box<Expr>),
    OpApply(Box<Expr>, String, Box<Expr>),
    Literal(LiteralData),
    Lambda(Pattern, Box<Expr>),
    // Case(Box<Expr>, NonEmpty<Alternative>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
    Tuple(NonEmpty<Expr>),
}

impl Expr {
    #[must_use]
    pub fn string_literal<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Literal(LiteralData::String(s.into()))
    }

    #[must_use]
    pub fn int_literal(int: i64) -> Expr {
        Expr::Literal(LiteralData::Integral(int))
    }

    #[must_use]
    pub fn float_literal(float: f64) -> Expr {
        Expr::Literal(LiteralData::fractional(float))
    }

    #[must_use]
    pub fn char_literal(c: char) -> Expr {
        Expr::Literal(LiteralData::Char(c))
    }

    #[must_use]
    pub fn identifier<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Identifier(s.into())
    }

    #[must_use]
    pub fn lambda<A>(args: A, body: Expr) -> Expr
    where
        A: Into<Vec<Pattern>>,
    {
        args.into()
            .into_iter()
            .rev()
            .fold(body, |body, arg| Expr::Lambda(arg, Box::new(body)))
    }

    #[must_use]
    pub fn bin_op<S, E1, E2>(op: S, first: E1, second: E2) -> Expr
    where
        S: Into<String>,
        E1: Into<Box<Expr>>,
        E2: Into<Box<Expr>>,
    {
        Expr::OpApply(first.into(), op.into(), second.into())
    }

    // #[must_use]
    // pub fn case<E>(expr: E, alts: Vec<Alternative>) -> Expr
    //     where
    //         E: Into<Box<Expr>>,
    // {
    //     if alts.is_empty() {
    //         return *expr.into();
    //     }
    //
    //     NonEmpty::try_from(alts)
    //         .map(|alts| Expr::Case(expr.into(), alts))
    //         .unwrap_or_else(|| *expr.into())
    // }

    #[must_use]
    pub fn if_then_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr
        where
            E: Into<Box<Expr>>,
            V1: Into<Box<Expr>>,
            V2: Into<Box<Expr>>,
    {
        Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
    }

    #[must_use]
    pub fn application<S, E>(address: NonEmpty<S>, args: E) -> Expr
    where
        S: Into<String>,
        E: Into<Vec<Expr>>,
    {
        let address = address.into_iter().map(Into::into).join("::");
        let func = Expr::Identifier(address);

        args.into()
            .into_iter()
            .fold(func, |func, arg| Expr::Apply(Box::new(func), arg.into()))
    }

    #[must_use]
    pub fn paren<E>(expr: E) -> Expr
    where
        E: Into<Box<Expr>>,
    {
        Expr::Paren(expr.into())
    }

    #[must_use]
    pub fn tuple<A>(args: A) -> Expr
    where
        A: Into<Vec<Expr>>,
    {
        let args = args.into();

        match &args[..] {
            [] => Expr::Unit,
            [arg] => Expr::paren(arg.clone()),
            _ => unsafe {
                Expr::Tuple(NonEmpty::new_unchecked(args))
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Pattern {
    Unit,
    Literal(LiteralData),
    Identifier(String),
    Tuple(NonEmpty<Pattern>),
    // Constructor(String, NonEmpty<Pattern>),
    // WildCard,
}

impl Pattern {
    #[must_use]
    pub fn string_literal<S>(s: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Literal(LiteralData::String(s.into()))
    }

    #[must_use]
    pub fn int_literal(int: i64) -> Pattern {
        Pattern::Literal(LiteralData::Integral(int))
    }

    #[must_use]
    pub fn float_literal(float: f64) -> Pattern {
        Pattern::Literal(LiteralData::fractional(float))
    }

    #[must_use]
    pub fn char_literal(c: char) -> Pattern {
        Pattern::Literal(LiteralData::Char(c))
    }

    #[must_use]
    pub fn identifier<S>(s: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Identifier(s.into())
    }

    #[must_use]
    pub fn tuple<A>(args: A) -> Pattern
    where
        A: Into<Vec<Pattern>>,
    {
        let args = args.into();

        match &args[..] {
            [] => Pattern::Unit,
            [arg] => arg.clone(),
            _ => unsafe {
                Pattern::Tuple(NonEmpty::new_unchecked(args))
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum LiteralData {
    Integral(i64),
    Fractional(NotNan<f64>),
    String(String),
    Char(char),
}

impl LiteralData {
    #[must_use]
    pub fn fractional(val: f64) -> Self {
        unsafe {
            LiteralData::Fractional(NotNan::new_unchecked(val))
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use pretty_assertions::{assert_eq, assert_ne};
}
