// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub enum Kind {
//     Function(Box<Kind>, Box<Kind>),
//     Star,
// }

// impl Kind {
//     pub fn new(v: isize) -> Kind {
//         match v {
//             0 => Kind::Star,
//             _ => Kind::Function( Box::new(Kind::Star), Box::new(Kind::Star)),
//         }
//     }
// }

use non_empty_vec::NonEmpty;
use std::convert::TryFrom;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct TypeConstructor {
    pub name: String,
    // pub kind: Kind,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct TypeVariable {
    pub id: String,
    // pub kind: Kind,
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

    pub(crate) fn variable<S>(s: S) -> Type
    where
        S: Into<String>,
    {
        Type::Variable(TypeVariable { id: s.into() })
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
//         assert_eq!(kind, Kind::Type,);
//     }
//
//     #[test]
//     fn test_new_kind_1() {
//         let kind = Kind::new(1);
//
//         assert_eq!(kind, Kind::function(Kind::Type, Kind::Type),);
//     }
//
//     #[test]
//     fn test_new_kind_2() {
//         let kind = Kind::new(2);
//
//         assert_eq!(
//             kind,
//             Kind::function(Kind::Type, Kind::function(Kind::Type, Kind::Type)),
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
//                 Kind::Type,
//                 Kind::function(Kind::Type, Kind::function(Kind::Type, Kind::Type))
//             ),
//         );
//     }
// }
