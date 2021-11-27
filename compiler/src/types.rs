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
        types: Vec<Type>,
    },
    // Named {
    //     type_name: String,
    //     target: Box<Type>,
    // },
    Tuple(Vec<Type>),
    // Unit,
}

impl Type {
    pub(crate) fn union(types: Vec<Type>) -> Type {
        Type::Union { types }
    }

    pub(crate) fn tuple(types: Vec<Type>) -> Type {
        Type::Tuple(types)
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
}
