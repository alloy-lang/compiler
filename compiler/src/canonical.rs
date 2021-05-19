use std::collections::BTreeSet;

use itertools::Itertools;

use crate::parse;

// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub(crate) enum BinOp {
//     Eq,
//     Ne,
//     Lt,
//     Le,
//     Gt,
//     Ge,
//     Add,
//     Sub,
//     Mul,
//     Div,
// }
//
// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub(crate) enum Expr {
//     Literal(String),
//     Identifier(String),
//     Assign(String, Box<Expr>),
//     Function(Vec<Expr>, Box<Expr>),
//     BinOp(BinOp, Box<Expr>, Box<Expr>),
//     IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
//     Call(Vec<String>, Vec<Expr>),
//     Tuple(Vec<Expr>),
// }
//
// impl Expr {
//     pub(crate) fn literal<S>(s: S) -> Expr where S: Into<String> {
//         Expr::Literal(s.into())
//     }
//
//     pub(crate) fn identifier<S>(s: S) -> Expr where S: Into<String> {
//         Expr::Identifier(s.into())
//     }
//
//     fn assign<S, E>(name: S, expr: E) -> Expr where S: Into<String>, E: Into<Box<Expr>> {
//         Expr::Assign(name.into(), expr.into())
//     }
//
//     fn function<A, E>(args: A, expr: E) -> Expr where A: Into<Vec<Expr>>, E: Into<Box<Expr>> {
//         Expr::Function(args.into(), expr.into())
//     }
//
//     pub(crate) fn bin_op<E>(op: BinOp, first: E, second: E) -> Expr where E: Into<Box<Expr>> {
//         Expr::BinOp(op, first.into(), second.into())
//     }
//
//     fn if_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr where E: Into<Box<Expr>>, V1: Into<Box<Expr>>, V2: Into<Box<Expr>> {
//         Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
//     }
//
//     pub(crate) fn call<E>(address: Vec<&str>, expr: E) -> Expr where E: Into<Vec<Expr>> {
//         let address = address.into_iter::<>()
//             .map(|s| String::from(s))
//             .collect::<Vec<_>>();
//
//         Expr::Call(address, expr.into())
//     }
// }
//
// impl From<Expr> for Vec<Expr> {
//     fn from(e: Expr) -> Self {
//         vec![e]
//     }
// }
//
// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub(crate) enum Type {
//     Identifier(String),
//     Atom(String),
//     Variable(String),
//     Lambda {
//         arg_type: Box<Type>,
//         return_type: Box<Type>,
//     },
//     Record {
//         properties: Vec<(String, Box<Type>)>,
//     },
//     Alias {
//         type_name: String,
//         target: Box<Type>,
//     },
//     Union {
//         types: Vec<Type>,
//     },
//     Named {
//         type_name: String,
//         target: Box<Type>,
//     },
//     Tuple(Vec<Type>),
//     Unit,
// }
//
// impl Type {
//     pub(crate) fn tuple(types: Vec<Type>) -> Type {
//         Type::Tuple(types)
//     }
//
//     fn lambda<T1, T2>(arg_type: T1, return_type: T2) -> Type where T1: Into<Box<Type>>, T2: Into<Box<Type>> {
//         Type::Lambda {
//             arg_type: arg_type.into(),
//             return_type: return_type.into(),
//         }
//     }
//
//     pub(crate) fn identifier<S>(s: S) -> Type where S: Into<String> {
//         Type::Identifier(s.into())
//     }
//
//     pub(crate) fn atom<S>(s: S) -> Type where S: Into<String> {
//         Type::Atom(s.into())
//     }
//
//     pub(crate) fn variable<S>(s: S) -> Type where S: Into<String> {
//         Type::Variable(s.into())
//     }
// }
//
// impl From<Type> for Vec<Type> {
//     fn from(t: Type) -> Self {
//         vec![t]
//     }
// }

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Declaration {
    Type {
        name: String,
        t: parse::Type,
    },
    Value {
        name: String,
        t: Option<parse::Type>,
        definition: parse::Expr,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Module {
    name: String,
    declarations: Vec<Declaration>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
struct TypeAnnotation {
    name: String,
    t: parse::Type,
}

impl TypeAnnotation {
    pub(crate) fn new<S>(name: S, t: parse::Type) -> TypeAnnotation
        where
            S: Into<String>,
    {
        TypeAnnotation {
            name: name.into(),
            t,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum CanonicalizeError {
    ConflictingTypeAnnotations {
        name: String,
        types: Vec<parse::Type>,
    },
}

pub(crate) fn canonicalize(parsed: parse::Module) -> Result<Module, Vec<CanonicalizeError>> {
    let (declarations, errors): (Vec<Result<_, _>>, Vec<Result<_, CanonicalizeError>>) = parsed
        .declarations
        .into_iter()
        .group_by(|dec| match dec {
            parse::Declaration::TypeAnnotation { name, t: _ } => name.clone(),
            parse::Declaration::Value {
                name,
                definition: _,
            } => name.clone(),
            parse::Declaration::TypeAliasDefinition {
                name,
                type_variables: _,
                t: _,
            } => name.clone(),
        })
        .into_iter()
        .map(|(name, declarations)| to_canonical_declaration(name, declarations))
        .partition(Result::is_ok);

    if !errors.is_empty() {
        let errors = errors
            .into_iter()
            .map(Result::unwrap_err)
            .collect::<Vec<_>>();
        return Err(errors);
    }

    let declarations = declarations
        .into_iter()
        .map(Result::unwrap)
        .flat_map(|f| f)
        .collect::<Vec<_>>();

    Ok(Module {
        name: parsed.name,
        declarations,
    })
}

fn to_canonical_declaration(
    name: String,
    declarations: impl IntoIterator<Item=parse::Declaration>,
) -> Result<Vec<Declaration>, CanonicalizeError> {
    let (type_hints, declarations): (BTreeSet<parse::Declaration>, BTreeSet<parse::Declaration>) =
        declarations
            .into_iter()
            .partition(|dec| matches!(dec, parse::Declaration::TypeAnnotation { .. }));

    let type_hint = match type_hints.len() {
        0 => None,
        1 => match type_hints.into_iter().next() {
            Some(parse::Declaration::TypeAnnotation { name: _, t }) => Some(t),
            _ => None,
        },
        _ => {
            return Err(CanonicalizeError::ConflictingTypeAnnotations {
                name,
                types: type_hints
                    .into_iter()
                    .filter_map(|dec| match dec {
                        parse::Declaration::TypeAnnotation { name, t } => Some(t),
                        _ => None,
                    })
                    .collect(),
            });
        }
    };

    let declarations = declarations
        .into_iter()
        .map(|declaration| match declaration {
            parse::Declaration::TypeAnnotation { name, t } => todo!("should never get here"),
            parse::Declaration::Value { name, definition } => Declaration::Value {
                name,
                t: type_hint.clone(),
                definition,
            },
            parse::Declaration::TypeAliasDefinition { .. } => todo!(),
            // parse::Declaration::TypeAliasDefinition { name, type_variables, t } => Declaration::Type {
            //     name,
            //     t: parse::Type::Unit,
            // },
        })
        .collect::<Vec<_>>();

    Ok(declarations)
}

#[cfg(test)]
mod tests {
    use crate::canonical;
    use crate::canonical::{canonicalize, Declaration, TypeAnnotation};
    use crate::parse;
    use crate::parse::{Expr, Type};

    macro_rules! assert_eq {
        ($expected:expr, $actual:expr $(,)?) => ({
            match (&$expected, &$actual) {
                (expected_val, actual_val) => {
                    if !(*expected_val == *actual_val) {
                        // The reborrows below are intentional. Without them, the stack slot for the
                        // borrow is initialized even before the values are compared, leading to a
                        // noticeable slow down.
                        panic!(r#"assertion failed: `(expected == actual)`
  expected: `{:?}`,
 actual: `{:?}`"#, &*expected_val, &*actual_val)
                    }
                }
            }
        });
        ($expected:expr, $actual:expr, $($arg:tt)+) => ({
            match (&$expected, &$actual) {
                (expected_val, actual_val) => {
                    if !(*expected_val == *actual_val) {
                        panic!($($arg)+)
                    }
                }
            }
        });
    }

    fn assert_declarations(
        module_name: String,
        expecteds: Vec<Declaration>,
        actuals: Vec<Declaration>,
    ) {
        let pairs = expecteds
            .iter()
            .zip(actuals.iter())
            .enumerate()
            .collect::<Vec<_>>();

        for (index, (expected, actual)) in pairs {
            assert_eq!(
                expected, actual,
                r#"

Declaration in module '{}' at index {} were not equal.
 expected: {:?}
   actual: {:?}
"#,
                module_name, index, expected, actual,
            );
        }

        assert_eq!(expecteds, actuals);
    }

    fn assert_module(expected: canonical::Module, actual: canonical::Module) {
        assert_eq!(
            expected.name, actual.name,
            r#"

Module names were not equal.
 expected: {}
   actual: {}
"#,
            expected.name, actual.name,
        );
        assert_declarations(expected.name, expected.declarations, actual.declarations);
    }

    #[test]
    fn test_empty_module() {
        let source: &str = r#"
            module Test
            where



"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![],
            },
            actual,
        );
    }

    #[test]
    fn test_value_declaration_no_type() {
        let source: &str = r#"
            module Test
            where

            thing = 0
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::Value {
                    name: String::from("thing"),
                    t: None,
                    definition: Expr::int_literal("0"),
                }],
            },
            actual,
        );
    }

    #[test]
    fn test_value_declaration_with_type() {
        let source: &str = r#"
            module Test
            where

            thing : Int
            thing = 0
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::Value {
                    name: String::from("thing"),
                    t: Some(Type::Identifier("Int".into())),
                    definition: Expr::int_literal("0"),
                }],
            },
            actual,
        );
    }

    #[test]
    fn test_value_declaration_with_conflicting_type_annotations() {
        let source: &str = r#"
            module Test
            where

            thing : String
            thing : Int
            thing = 0
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap_err();

        assert_eq!(
            vec![canonical::CanonicalizeError::ConflictingTypeAnnotations {
                name: "thing".into(),
                types: vec![
                    parse::Type::identifier("Int"),
                    parse::Type::identifier("String"),
                ],
            }],
            actual,
        );
    }

//     #[test]
//     fn test_single_arg_function_declaration_with_type() {
//         let source: &str = r#"
//             module Test
//             where
//
//             increment_positive : Int -> Int
//             increment_positive = |0| => 0
//             increment_positive = |x| => x + 1
// "#;
//         let parsed_module = parse::parser::module(source).unwrap();
//         let actual = canonicalize(parsed_module).unwrap();
//
//         assert_module(
//             canonical::Module {
//                 name: String::from("Test"),
//                 declarations: vec![
//                     Declaration::Value {
//                         name: String::from("increment_positive"),
//                         t: Type::lambda(
//                             Type::identifier("Int"),
//                             Type::identifier("Int"),
//                         ),
//                         definition: Expr::function(
//                             Expr::literal("0"),
//                             Expr::literal("0"),
//                         ),
//                     },
//                     Declaration::Value {
//                         name: String::from("increment_positive"),
//                         t: Type::lambda(
//                             Type::identifier("Int"),
//                             Type::identifier("Int"),
//                         ),
//                         definition: Expr::function(
//                             Expr::identifier("x"),
//                             Expr::bin_op(
//                                 BinOp::Add,
//                                 Expr::identifier("x"),
//                                 Expr::literal("1"),
//                             ),
//                         ),
//                     }
//                 ],
//             },
//             actual,
//         );
//     }
//
//     #[test]
//     fn test_multi_arg_function_declaration_with_type() {
//         let source: &str = r#"
//             module Test
//             where
//
//             increment_by_length : (Int, String) -> Int
//             increment_by_length = |(0, 1)| => 0
//             increment_by_length = |(x, y)| => x + String::length(y)
// "#;
//         let parsed_module = parse::parser::module(source).unwrap();
//         let actual = canonicalize(parsed_module).unwrap();
//
//         assert_module(
//             canonical::Module {
//                 name: String::from("Test"),
//                 declarations: vec![
//                     Declaration::TypeAnnotation {
//                         name: String::from("increment_by_length"),
//                         t: Type::lambda(
//                             Type::tuple(vec![
//                                 Type::identifier("Int"),
//                                 Type::identifier("String"),
//                             ]),
//                             Type::identifier("Int"),
//                         ),
//                     },
//                     Declaration::Value {
//                         name: String::from("increment_by_length"),
//                         definition: Expr::function(
//                             Expr::Tuple(vec![Expr::literal("0"), Expr::literal("1")]),
//                             Expr::literal("0"),
//                         ),
//                     },
//                     Declaration::Value {
//                         name: String::from("increment_by_length"),
//                         definition: Expr::function(
//                             Expr::Tuple(vec![Expr::identifier("x"), Expr::identifier("y")]),
//                             Expr::bin_op(
//                                 BinOp::Add,
//                                 Expr::identifier("x"),
//                                 Expr::call(
//                                     vec!["String", "length"],
//                                     Expr::identifier("y"),
//                                 ),
//                             ),
//                         ),
//                     }
//                 ],
//             },
//             actual,
//         );
//     }
//
//     #[test]
//     fn test_curried_function_declaration_with_type() {
//         let source: &str = r#"
//             module Test
//             where
//
//             increment_by_length : (Int -> Int) -> Int -> Int
//             increment_by_length = |f| => |value| => f(value)
// "#;
//         let parsed_module = parse::parser::module(source).unwrap();
//         let actual = canonicalize(parsed_module).unwrap();
//
//         assert_module(
//             canonical::Module {
//                 name: String::from("Test"),
//                 declarations: vec![
//                     Declaration::TypeAnnotation {
//                         name: String::from("increment_by_length"),
//                         t: Type::lambda(
//                             Type::lambda(
//                                 Type::identifier("Int"),
//                                 Type::identifier("Int"),
//                             ),
//                             Type::lambda(
//                                 Type::identifier("Int"),
//                                 Type::identifier("Int"),
//                             ),
//                         ),
//                     },
//                     Declaration::Value {
//                         name: String::from("increment_by_length"),
//                         definition: Expr::function(
//                             Expr::identifier("f"),
//                             Expr::function(
//                                 Expr::identifier("value"),
//                                 Expr::call(
//                                     vec!["f"],
//                                     Expr::identifier("value"),
//                                 ),
//                             ),
//                         ),
//                     }
//                 ],
//             },
//             actual,
//         );
//     }
//
//     #[test]
//     fn test_simple_if_then_else() {
//         let source: &str = r#"
//             module Test
//             where
//
//             increment_positive = |num| =>
//               if Number::is_positive?(num)
//               then num + 1
//               else num
//             decrement_negative = |num| =>
//               if Number::is_negative?(num)
//               then num - 1
//               else num
// "#;
//         let parsed_module = parse::parser::module(source).unwrap();
//         let actual = canonicalize(parsed_module).unwrap();
//
//         assert_module(
//             canonical::Module {
//                 name: String::from("Test"),
//                 declarations: vec![
//                     Declaration::Value {
//                         name: String::from("increment_positive"),
//                         definition: Expr::function(
//                             Expr::identifier("num"),
//                             Expr::if_else(
//                                 Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
//                                 Expr::bin_op(
//                                     BinOp::Add,
//                                     Expr::identifier("num"),
//                                     Expr::literal("1"),
//                                 ),
//                                 Expr::identifier("num"),
//                             ),
//                         ),
//                     },
//                     Declaration::Value {
//                         name: String::from("decrement_negative"),
//                         definition: Expr::function(
//                             Expr::identifier("num"),
//                             Expr::if_else(
//                                 Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
//                                 Expr::bin_op(
//                                     BinOp::Sub,
//                                     Expr::identifier("num"),
//                                     Expr::literal("1"),
//                                 ),
//                                 Expr::identifier("num"),
//                             ),
//                         ),
//                     },
//                 ],
//             },
//             actual,
//         );
//     }
//
//     #[test]
//     fn test_nested_if_then_else() {
//         let source: &str = r#"
//             module Test
//             where
//
//             increment_or_decrement = |num| =>
//               if Number::is_positive?(num)
//               then num + 1
//               else
//                 if Number::is_negative?(num)
//                 then num - 1
//                 else num
// "#;
//         let parsed_module = parse::parser::module(source).unwrap();
//         let actual = canonicalize(parsed_module).unwrap();
//
//         assert_module(
//             canonical::Module {
//                 name: String::from("Test"),
//                 declarations: vec![
//                     Declaration::Value {
//                         name: String::from("increment_or_decrement"),
//                         definition: Expr::function(
//                             Expr::identifier("num"),
//                             Expr::if_else(
//                                 Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
//                                 Expr::bin_op(
//                                     BinOp::Add,
//                                     Expr::identifier("num"),
//                                     Expr::literal("1"),
//                                 ),
//                                 Expr::if_else(
//                                     Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
//                                     Expr::bin_op(
//                                         BinOp::Sub,
//                                         Expr::identifier("num"),
//                                         Expr::literal("1"),
//                                     ),
//                                     Expr::identifier("num"),
//                                 ),
//                             ),
//                         ),
//                     },
//                 ],
//             },
//             actual,
//         );
//     }
//
//     #[test]
//     fn test_multi_property_union_type() {
//         let expected = canonical::Module {
//             name: String::from("Test"),
//             declarations: vec![
//                 Declaration::TypeAliasDefinition {
//                     name: String::from("Either"),
//                     type_variables: vec![String::from("L"), String::from("R")],
//                     t: Type::Union {
//                         types: vec![
//                             Type::Tuple(vec![
//                                 Type::atom("Right"),
//                                 Type::identifier("R"),
//                             ]),
//                             Type::Tuple(vec![
//                                 Type::atom("Left"),
//                                 Type::identifier("L"),
//                             ]),
//                         ]
//                     },
//                 },
//             ],
//         };
//
//         {
//             let source: &str = r#"
//             module Test
//             where
//
//             data Either<L, R> =
//               | (:Right, R)
//               | (:Left, L)
// "#;
//
//             let parsed_module = parse::parser::module(source).unwrap();
//             let actual = canonicalize(parsed_module).unwrap();
//
//             assert_module(
//                 expected.clone(),
//                 actual,
//             );
//         }
//         {
//             let source: &str = r#"
//             module Test
//             where
//
//             data Either<L, R> =
//               (:Right, R)
//               | (:Left, L)
// "#;
//
//             let parsed_module = parse::parser::module(source).unwrap();
//             let actual = canonicalize(parsed_module).unwrap();
//
//             assert_module(
//                 expected.clone(),
//                 actual,
//             );
//         }
//         {
//             let source: &str = r#"
//             module Test
//             where
//
//             data Either<L, R> = (:Right, R) | (:Left, L)
// "#;
//
//             let parsed_module = parse::parser::module(source).unwrap();
//             let actual = canonicalize(parsed_module).unwrap();
//
//             assert_module(
//                 expected,
//                 actual,
//             );
//         }
//     }
}
