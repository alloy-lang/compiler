use itertools::{Either, Itertools};

use crate::parse;
use crate::parse::Expr;
use crate::type_inference::infer_type;

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
struct TypeAlias {
    name: String,
    type_variables: Vec<String>,
    t: parse::Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
struct Value {
    name: String,
    t: Option<parse::Type>,
    definition: Expr,
}

impl Value {
    fn new_value<S>(name: S, t: parse::Type, definition: Expr) -> Value
    where
        S: Into<String>,
    {
        Value {
            name: name.into(),
            t: Some(t),
            definition,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Module {
    name: String,
    values: Vec<Value>,
    type_aliases: Vec<TypeAlias>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum CanonicalizeError {
    MissingTypeAnnotation {
        name: String,
    },
    ConflictingTypeAnnotations {
        name: String,
        types: Vec<parse::TypeAnnotation>,
    },
    ConflictingTypeAliasDefinitions {
        name: String,
        type_aliases: Vec<parse::TypeAliasDefinition>,
    },
    MissingValueDefinition {
        name: String,
        type_hint: Option<parse::Type>,
    },
}

pub(crate) fn canonicalize(parsed: parse::Module) -> Result<Module, Vec<CanonicalizeError>> {
    let parse::Module {
        name,
        type_annotations,
        values,
        type_aliases,
    } = parsed;

    let (values, value_errors): (Vec<Value>, Vec<CanonicalizeError>) = values
        .into_iter()
        .group_by(|v| v.name.clone())
        .into_iter()
        .map(|(name, values)| to_canonical_value(name, values, type_annotations.clone()))
        .partition_map(|res| match res {
            Ok(value) => Either::Left(value),
            Err(error) => Either::Right(error),
        });

    let (type_aliases, type_alias_errors): (Vec<TypeAlias>, Vec<CanonicalizeError>) = type_aliases
        .into_iter()
        .group_by(|ta| ta.name.clone())
        .into_iter()
        .map(|(name, declarations)| to_canonical_type_alias(name, declarations))
        .partition_map(|res| match res {
            Ok(value) => Either::Left(value),
            Err(error) => Either::Right(error),
        });

    let errors: Vec<CanonicalizeError> = vec![value_errors, type_alias_errors]
        .into_iter()
        .flatten()
        .collect::<Vec<CanonicalizeError>>();

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(Module {
        name,
        values,
        type_aliases,
    })
}

fn to_canonical_value(
    name: String,
    values: impl IntoIterator<Item = parse::Value>,
    type_annotations: impl IntoIterator<Item = parse::TypeAnnotation>,
) -> Result<Value, CanonicalizeError> {
    let values = values.into_iter().collect::<Vec<_>>();
    let type_annotations = type_annotations
        .into_iter()
        .filter(|ta| ta.name == name)
        .collect::<Vec<_>>();

    if type_annotations.len() > 1 {
        return Err(CanonicalizeError::ConflictingTypeAnnotations {
            name,
            types: type_annotations,
        });
    }

    let type_hint = type_annotations
        .first()
        .map(|parse::TypeAnnotation { t, .. }| t.clone());

    if values.is_empty() {
        return Err(CanonicalizeError::MissingValueDefinition { name, type_hint });
    }

    let definition = values
        .into_iter()
        .map(|value| value.definition)
        .collect::<Expr>();

    let type_def = type_hint.unwrap_or_else(|| infer_type(&definition));

    Ok(Value {
        name,
        t: Some(type_def),
        definition,
    })
}

fn to_canonical_type_alias(
    name: String,
    type_aliases: impl IntoIterator<Item = parse::TypeAliasDefinition>,
) -> Result<TypeAlias, CanonicalizeError> {
    let type_aliases = type_aliases.into_iter().collect::<Vec<_>>();

    match &type_aliases[..] {
        [parse::TypeAliasDefinition {
            name,
            type_variables,
            t,
        }] => Ok(TypeAlias {
            name: name.clone(),
            type_variables: type_variables.clone(),
            t: t.clone(),
        }),
        [] => panic!("Empty TypeAlias list for name: {}", name),
        // _ => Err(CanonicalizeError::ConflictingTypeAliasDefinitions { name, type_aliases }),
        _ => todo!("conflicting TypeAlias definitions"),
    }
}

#[cfg(test)]
mod tests {
    use crate::canonical;
    use crate::canonical::canonicalize;
    use crate::parse;
    use crate::parse::{Expr, Type};
    use crate::test_source;

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

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_eq!(
            canonical::Module {
                name: String::from("Test"),
                values: vec![],
                type_aliases: vec![],
            },
            actual,
        );
    }

    #[test]
    fn test_int_value_declaration_with_or_without_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
                "thing",
                parse::Type::identifier("Int"),
                Expr::int_literal("0"),
            )],
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::INT_VALUE_DECLARATION_WITH_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
        {
            let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_float_value_declaration_with_or_without_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
                "thing",
                parse::Type::identifier("Float"),
                Expr::float_literal("0.1"),
            )],
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
        {
            let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_value_declaration_with_conflicting_type_annotations() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_CONFLICTING_TYPE_ANNOTATIONS;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap_err();

        assert_eq!(
            vec![canonical::CanonicalizeError::ConflictingTypeAnnotations {
                name: "thing".into(),
                types: vec![
                    parse::TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: parse::Type::identifier("String"),
                    },
                    parse::TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: parse::Type::identifier("Int"),
                    },
                ],
            }],
            actual,
        );
    }

    // #[test]
    // fn test_type_annotation_with_no_definition_returns_error() {
    //     let source: &str = test_source::TYPE_ANNOTATION_WITH_NO_DEFINITION;
    //     let parsed_module = parse::parser::module(source).unwrap();
    //     let actual = canonicalize(parsed_module).unwrap_err();
    //
    //     assert_eq!(
    //         vec![canonical::CanonicalizeError::MissingValueDefinition {
    //             name: "thing".into(),
    //             type_hint: parse::Type::identifier("String").into(),
    //         }],
    //         actual,
    //     );
    // }

    #[test]
    fn test_single_arg_function_declaration_with_or_without_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
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
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }

        {
            let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
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
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
        {
            let source: &str = test_source::MULTI_ARG_FUNCTION_DECLARATION_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
                String::from("apply"),
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
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_curried_function_declaration_with_no_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new_value(
                String::from("apply"),
                Type::lambda(
                    Type::lambda(Type::variable("T3"), Type::variable("T4")),
                    Type::lambda(Type::variable("T3"), Type::variable("T4")),
                ),
                Expr::function(
                    Expr::identifier("f"),
                    Expr::function(
                        Expr::identifier("value"),
                        Expr::call(vec!["f"], Expr::identifier("value")),
                    ),
                ),
            )],
            type_aliases: vec![],
        };

        {
            let source: &str = test_source::CURRIED_FUNCTION_DECLARATION_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_simple_if_then_else() {
        let source: &str = test_source::SIMPLE_IF_THEN_ELSE;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_eq!(
            canonical::Module {
                name: String::from("Test"),
                values: vec![
                    canonical::Value::new_value(
                        String::from("increment_positive"),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
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
                    canonical::Value::new_value(
                        String::from("decrement_negative"),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
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
                type_aliases: vec![],
            },
            actual,
        );
    }

    #[test]
    fn test_nested_if_then_else() {
        let source: &str = test_source::NESTED_IF_THEN_ELSE;
        let parsed_module = parse::parser::module(source).unwrap();
        let actual = canonicalize(parsed_module).unwrap();

        assert_eq!(
            canonical::Module {
                name: String::from("Test"),
                values: vec![canonical::Value::new_value(
                    String::from("increment_or_decrement"),
                    Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
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
                type_aliases: vec![],
            },
            actual,
        );
    }

    #[test]
    fn test_multi_property_union_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![],
            type_aliases: vec![canonical::TypeAlias {
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
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_1;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected.clone(), actual);
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_2;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected.clone(), actual);
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_3;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected, actual);
        }
    }
}
