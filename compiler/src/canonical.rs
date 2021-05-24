use std::collections::BTreeSet;

use itertools::{Either, Itertools};

use crate::parse;
use crate::parse::Expr;

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
pub(crate) struct TypeAnnotation {
    name: String,
    type_variables: Vec<String>,
    t: parse::Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Value {
    name: String,
    definition: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeAliasDefinition {
    name: String,
    type_variables: Vec<String>,
    t: parse::Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Declaration {
    Type {
        name: String,
        type_variables: Vec<String>,
        t: parse::Type,
    },
    Value {
        name: String,
        t: Option<parse::Type>,
        definition: Expr,
    },
}

impl Declaration {
    fn new_value<S>(name: S, t: parse::Type, definition: Expr) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::Value {
            name: name.into(),
            t: Some(t),
            definition,
        }
    }

    fn new_value_no_type<S>(name: S, definition: Expr) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::Value {
            name: name.into(),
            t: None,
            definition,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Module {
    name: String,
    declarations: Vec<Declaration>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum CanonicalizeError {
    ConflictingTypeAnnotations {
        name: String,
        types: Vec<TypeAnnotation>,
    },
    ConflictingTypeAliasDefinitions {
        name: String,
        type_aliases: Vec<TypeAliasDefinition>,
    },
    MissingValueDefinition {
        name: String,
        type_hint: Option<parse::Type>,
    },
}

pub(crate) fn canonicalize(parsed: parse::Module) -> Result<Module, Vec<CanonicalizeError>> {
    let (declarations, errors): (Vec<Result<_, _>>, Vec<Result<_, CanonicalizeError>>) = parsed
        .declarations
        .into_iter()
        .group_by(extract_name)
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
        .collect::<Vec<_>>();

    Ok(Module {
        name: parsed.name,
        declarations,
    })
}

fn extract_name(dec: &parse::Declaration) -> String {
    match dec {
        parse::Declaration::TypeAnnotation { name, .. } => name.clone(),
        parse::Declaration::Value { name, .. } => name.clone(),
        parse::Declaration::TypeAliasDefinition { name, .. } => name.clone(),
    }
}

fn to_canonical_declaration(
    name: String,
    declarations: impl IntoIterator<Item = parse::Declaration>,
) -> Result<Declaration, CanonicalizeError> {
    let (type_annotations, values, type_aliases): (
        Vec<TypeAnnotation>,
        Vec<Value>,
        Vec<TypeAliasDefinition>,
    ) = {
        let (type_annotations, declarations): (Vec<TypeAnnotation>, Vec<parse::Declaration>) =
            declarations.into_iter().partition_map(|dec| match dec {
                parse::Declaration::TypeAnnotation {
                    name,
                    type_variables,
                    t,
                } => Either::Left(TypeAnnotation {
                    name,
                    type_variables,
                    t,
                }),
                parse::Declaration::Value { .. } => Either::Right(dec),
                parse::Declaration::TypeAliasDefinition { .. } => Either::Right(dec),
            });
        let (values, type_alias_definitions): (Vec<Value>, Vec<TypeAliasDefinition>) =
            declarations.into_iter().partition_map(|dec| match dec {
                parse::Declaration::TypeAnnotation { .. } => todo!("should never get here"),
                parse::Declaration::Value { name, definition } => {
                    Either::Left(Value { name, definition })
                }
                parse::Declaration::TypeAliasDefinition {
                    name,
                    type_variables,
                    t,
                } => Either::Right(TypeAliasDefinition {
                    name,
                    type_variables,
                    t,
                }),
            });

        (type_annotations, values, type_alias_definitions)
    };

    if type_aliases.len() > 1 {
        return Err(CanonicalizeError::ConflictingTypeAliasDefinitions { name, type_aliases });
    }

    if let Some(TypeAliasDefinition {
        name,
        type_variables,
        t,
    }) = type_aliases.first().map(Clone::clone)
    {
        return Ok(Declaration::Type {
            name,
            type_variables,
            t,
        });
    }

    if type_annotations.len() > 1 {
        return Err(CanonicalizeError::ConflictingTypeAnnotations {
            name,
            types: type_annotations,
        });
    }

    let type_hint = type_annotations
        .first()
        .map(|TypeAnnotation { t, .. }| t.clone());

    if values.is_empty() {
        return Err(CanonicalizeError::MissingValueDefinition { name, type_hint });
    }

    let definition = values
        .into_iter()
        .map(|value| value.definition)
        .collect::<Expr>();

    Ok(Declaration::Value {
        name,
        t: type_hint,
        definition,
    })
}

#[cfg(test)]
mod tests {
    use crate::canonical;
    use crate::canonical::{canonicalize, Declaration};
    use crate::parse;
    use crate::parse::{BinOp, Expr, Type};

    use super::TypeAnnotation;

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
                    TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: parse::Type::identifier("String"),
                    },
                    TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: parse::Type::identifier("Int"),
                    },
                ],
            }],
            actual,
        );
    }

    #[test]
    fn test_type_annotation_with_no_definition_returns_error() {
        let source: &str = r#"
            module Test
            where

            thing : String
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap_err();

        assert_eq!(
            vec![canonical::CanonicalizeError::MissingValueDefinition {
                name: "thing".into(),
                type_hint: parse::Type::identifier("String").into(),
            }],
            actual,
        );
    }

    #[test]
    fn test_single_arg_function_declaration_with_type() {
        let source: &str = r#"
            module Test
            where

            increment_positive : Int -> Int
            increment_positive = |0| => 0
            increment_positive = |x| => x + 1
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::new_value(
                    String::from("increment_positive"),
                    Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    Expr::Match(vec![
                        Expr::function(Expr::int_literal("0"), Expr::int_literal("0")),
                        Expr::function(
                            Expr::identifier("x"),
                            Expr::bin_op(
                                parse::BinOp::Add,
                                Expr::identifier("x"),
                                Expr::int_literal("1"),
                            ),
                        ),
                    ]),
                )],
            },
            actual,
        );
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source: &str = r#"
            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, "")| => 0
            increment_by_length = |(x, y)| => x + String::length(y)
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::new_value(
                    String::from("increment_by_length"),
                    Type::lambda(
                        Type::tuple(vec![Type::identifier("Int"), Type::identifier("String")]),
                        Type::identifier("Int"),
                    ),
                    Expr::Match(vec![
                        Expr::function(
                            Expr::Tuple(vec![Expr::int_literal("0"), Expr::string_literal("")]),
                            Expr::int_literal("0"),
                        ),
                        Expr::function(
                            Expr::Tuple(vec![Expr::identifier("x"), Expr::identifier("y")]),
                            Expr::bin_op(
                                parse::BinOp::Add,
                                Expr::identifier("x"),
                                Expr::call(vec!["String", "length"], Expr::identifier("y")),
                            ),
                        ),
                    ]),
                )],
            },
            actual,
        );
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let source: &str = r#"
            module Test
            where

            increment_by_length : (Int -> Int) -> Int -> Int
            increment_by_length = |f| => |value| => f(value)
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::new_value(
                    String::from("increment_by_length"),
                    Type::lambda(
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    ),
                    Expr::function(
                        Expr::identifier("f"),
                        Expr::function(
                            Expr::identifier("value"),
                            Expr::call(vec!["f"], Expr::identifier("value")),
                        ),
                    ),
                )],
            },
            actual,
        );
    }

    #[test]
    fn test_simple_if_then_else() {
        let source: &str = r#"
            module Test
            where

            increment_positive = |num| =>
              if Number::is_positive?(num)
              then num + 1
              else num
            decrement_negative = |num| =>
              if Number::is_negative?(num)
              then num - 1
              else num
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::new_value_no_type(
                        String::from("increment_positive"),
                        Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    parse::BinOp::Add,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    ),
                    Declaration::new_value_no_type(
                        String::from("decrement_negative"),
                        Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    parse::BinOp::Sub,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    ),
                ],
            },
            actual,
        );
    }

    #[test]
    fn test_nested_if_then_else() {
        let source: &str = r#"
        module Test
        where

        increment_or_decrement = |num| =>
          if Number::is_positive?(num)
          then num + 1
          else
            if Number::is_negative?(num)
            then num - 1
            else num
"#;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_module(
            canonical::Module {
                name: String::from("Test"),
                declarations: vec![Declaration::new_value_no_type(
                    String::from("increment_or_decrement"),
                    Expr::function(
                        Expr::identifier("num"),
                        Expr::if_else(
                            Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                            Expr::bin_op(
                                parse::BinOp::Add,
                                Expr::identifier("num"),
                                Expr::int_literal("1"),
                            ),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    parse::BinOp::Sub,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    ),
                )],
            },
            actual,
        );
    }

    #[test]
    fn test_multi_property_union_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            declarations: vec![Declaration::Type {
                name: String::from("Either"),
                type_variables: vec![String::from("L"), String::from("R")],
                t: Type::Union {
                    types: vec![
                        Type::Tuple(vec![Type::atom("Right"), Type::identifier("R")]),
                        Type::Tuple(vec![Type::atom("Left"), Type::identifier("L")]),
                    ],
                },
            }],
        };

        {
            let source: &str = r#"
                module Test
                where

                data Either<L, R> =
                  | (:Right, R)
                  | (:Left, L)
    "#;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_module(expected.clone(), actual);
        }
        {
            let source: &str = r#"
                module Test
                where

                data Either<L, R> =
                  (:Right, R)
                  | (:Left, L)
    "#;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_module(expected.clone(), actual);
        }
        {
            let source: &str = r#"
                module Test
                where

                data Either<L, R> = (:Right, R) | (:Left, L)
    "#;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_module(expected, actual);
        }
    }
}
