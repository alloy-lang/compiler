// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub(crate) enum Kind {
//     Function(Box<Kind>, Box<Kind>),
//     Concrete,
//     Constraint,
// }
//
// impl Kind {
//     pub(crate) fn new(v: usize) -> Kind {
//         match v {
//             0 => Kind::Concrete,
//             _ => Kind::rec_new(v - 1, Kind::Concrete),
//         }
//     }
//
//     fn rec_new(v: usize, kind: Kind) -> Kind {
//         let kind = Kind::function(Kind::Concrete, kind);
//
//         match v {
//             0 => kind,
//             _ => Kind::rec_new(v - 1, kind),
//         }
//     }
//
//     pub(crate) fn function<K>(l: K, r: K) -> Kind
//     where
//         K: Into<Box<Kind>>,
//     {
//         Kind::Function(l.into(), r.into())
//     }
// }

use non_empty_vec::NonEmpty;
use std::convert::TryFrom;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeConstructor {
    pub(crate) name: String,
    // pub(crate) kind: Kind,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeVariable {
    pub(crate) id: String,
    // pub(crate) kind: Kind,
}

impl TypeVariable {
    pub(crate) fn new_type<S>(id: S) -> Self
    where
        S: Into<String>,
    {
        TypeVariable {
            id: id.into(),
            // kind: Kind::Concrete,
        }
    }
    pub(crate) fn new_type_function<S>(id: S, args: usize) -> Self
    where
        S: Into<String>,
    {
        TypeVariable {
            id: id.into(),
            // kind: Kind::new(args),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Type {
    Identifier(String),
    Atom(String),
    Variable(TypeVariable),
    Bound {
        t: Box<Type>,
        binds: Vec<Type>,
    },
    Lambda {
        arg_type: Box<Type>,
        return_type: Box<Type>,
    },
    // Record {
    //     properties: Vec<(String, Box<Type>)>,
    // },
    // Alias {
    //     type_name: String,
    //     target: Box<Type>,
    // },
    Union {
        types: NonEmpty<Type>,
    },
    // Named {
    //     type_name: String,
    //     target: Box<Type>,
    // },
    Tuple(NonEmpty<Type>),
    // Unit,
}

impl Type {
    pub(crate) fn union(types: Vec<Type>) -> Type {
        Type::Union {
            types: NonEmpty::try_from(types).unwrap(),
        }
    }

    pub(crate) fn tuple(types: Vec<Type>) -> Type {
        Type::Tuple(NonEmpty::try_from(types).unwrap())
    }

    pub(crate) fn lambda<T1, T2>(arg_type: T1, return_type: T2) -> Type
    where
        T1: Into<Box<Type>>,
        T2: Into<Box<Type>>,
    {
        Type::Lambda {
            arg_type: arg_type.into(),
            return_type: return_type.into(),
        }
    }

    pub(crate) fn identifier<S>(s: S) -> Type
    where
        S: Into<String>,
    {
        Type::Identifier(s.into())
    }

    pub(crate) fn atom<S>(s: S) -> Type
    where
        S: Into<String>,
    {
        Type::Atom(s.into())
    }

    pub(crate) fn variable<S>(id: S) -> Type
    where
        S: Into<String>,
    {
        Type::Variable(TypeVariable::new_type(id))
    }

    pub(crate) fn bound<T>(t: T, binds: Vec<Type>) -> Type
    where
        T: Into<Box<Type>>,
    {
        Type::Bound { t: t.into(), binds }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::Kind;
//
//     #[test]
//     fn test_new_kind_0() {
//         let kind = Kind::new(0);
//
//         assert_eq!(kind, Kind::Concrete);
//     }
//
//     #[test]
//     fn test_new_kind_1() {
//         let kind = Kind::new(1);
//
//         assert_eq!(
//             kind,
//             Kind::function(Kind::Concrete, Kind::Concrete),
//         );
//     }
//
//     #[test]
//     fn test_new_kind_2() {
//         let kind = Kind::new(2);
//
//         assert_eq!(
//             kind,
//             Kind::function(Kind::Concrete, Kind::function(Kind::Concrete, Kind::Concrete)),
//         );
//     }
//
//     #[test]
//     fn test_new_kind_3() {
//         let kind = Kind::new(3);
//
//         assert_eq!(
//             kind,
//             Kind::function(
//                 Kind::Concrete,
//                 Kind::function(Kind::Concrete, Kind::function(Kind::Concrete, Kind::Concrete)),
//             ),
//         );
//     }
// }
