use itertools::{Either, Itertools};

use crate::parse;
use crate::parse::{TypeAliasDefinition, TypeAnnotation};
use crate::type_inference::{infer_type, TypeEnvironment};
use crate::types::Type;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
struct TypeAlias {
    name: String,
    type_variables: Vec<String>,
    t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
struct Value {
    name: String,
    t: Type,
    definition: parse::Expr,
}

impl Value {
    fn new<S>(name: S, t: Type, definition: parse::Expr) -> Value
    where
        S: Into<String>,
    {
        Value {
            name: name.into(),
            t,
            definition,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Module {
    name: String,
    values: Vec<Value>,
    type_aliases: Vec<TypeAlias>,
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
        type_hint: Option<Type>,
    },
}

pub(crate) fn canonicalize(parsed: parse::Module) -> Result<Module, Vec<CanonicalizeError>> {
    let parse::Module {
        name,
        type_annotations,
        values,
        type_aliases,
    } = parsed;

    let mut type_map = TypeEnvironment::new();
    let (values, value_errors): (Vec<Value>, Vec<CanonicalizeError>) = values
        .into_iter()
        .group_by(|v| v.name.clone())
        .into_iter()
        .map(|(name, values)| {
            to_canonical_value(name, values, type_annotations.clone(), &type_map).map(|v| {
                type_map.insert_expr_type_by_name(&v.name, v.t.clone());
                v
            })
        })
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
    type_annotations: impl IntoIterator<Item = TypeAnnotation>,
    type_map: &TypeEnvironment,
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
        .map(|TypeAnnotation { t, .. }| t.clone());

    let definition = match &values[..] {
        [] => return Err(CanonicalizeError::MissingValueDefinition { name, type_hint }),
        [value] => value.definition.clone(),
        _ => parse::Expr::lambda(
            parse::Pattern::identifier("a"),
            parse::Expr::case(
                parse::Expr::identifier("a"),
                values
                    .into_iter()
                    .map(|value| {
                        match value.definition {
                            parse::Expr::Paren(_) => todo!("parse::Expr::Paren(expr)"),
                            // parse::Expr::Paren(expr) => parse::Alternative {
                            //     pattern: parse::Pattern::WildCard,
                            //     matches: parse::Match::Simple(*expr),
                            // },
                            parse::Expr::Identifier(_) => todo!("parse::Expr::Identifier(_)"),
                            parse::Expr::Apply(_, _) => todo!("parse::Expr::Apply(_, _)"),
                            parse::Expr::OpApply(_, _, _) => todo!("parse::Expr::OpApply(_, _, _)"),
                            parse::Expr::Literal(_) => todo!("parse::Expr::Literal(_)"),
                            parse::Expr::Lambda(pattern, expr) => parse::Alternative {
                                pattern,
                                matches: parse::Match::Simple(*expr),
                            },
                            parse::Expr::Case(_, _) => todo!("parse::Expr::Case(_, _)"),
                            parse::Expr::IfElse(_, _, _) => todo!("parse::Expr::IfElse(_, _, _)"),
                        }
                    })
                    .collect(),
            ),
        ),
    };

    let type_def = type_hint.unwrap_or_else(|| infer_type(&definition, type_map));

    Ok(Value::new(name, type_def, definition))
}

fn to_canonical_type_alias(
    name: String,
    type_aliases: impl IntoIterator<Item = TypeAliasDefinition>,
) -> Result<TypeAlias, CanonicalizeError> {
    let type_aliases = type_aliases.into_iter().collect::<Vec<_>>();

    match &type_aliases[..] {
        [TypeAliasDefinition {
            name,
            type_variables,
            t,
        }] => Ok(TypeAlias {
            name: name.clone(),
            type_variables: type_variables.clone(),
            t: t.clone(),
        }),
        [] => panic!("Empty TypeAlias list for name: {}", name),
        _ => Err(CanonicalizeError::ConflictingTypeAliasDefinitions { name, type_aliases }),
    }
}

#[cfg(test)]
mod tests {
    use crate::canonical;
    use crate::canonical::canonicalize;
    use crate::parse;
    use crate::parse::{Alternative, Expr, Match, Pattern, TypeAliasDefinition, TypeAnnotation};
    use crate::test_source;
    use crate::types::Type;

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
            values: vec![canonical::Value::new(
                "thing",
                Type::identifier("Int"),
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
            values: vec![canonical::Value::new(
                "thing",
                Type::identifier("Float"),
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
                    TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: Type::identifier("String"),
                    },
                    TypeAnnotation {
                        name: "thing".into(),
                        type_variables: vec![],
                        t: Type::identifier("Int"),
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
    //             type_hint: Type::identifier("String").into(),
    //         }],
    //         actual,
    //     );
    // }

    #[test]
    fn test_single_arg_function_declaration_with_or_without_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new(
                String::from("increment_positive"),
                Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                Expr::lambda(
                    Pattern::identifier("a"),
                    Expr::case(
                        Expr::identifier("a"),
                        vec![
                            Alternative {
                                pattern: Pattern::int_literal("0"),
                                matches: Match::Simple(Expr::int_literal("0")),
                            },
                            Alternative {
                                pattern: Pattern::identifier("x"),
                                matches: Match::Simple(Expr::bin_op(
                                    "+",
                                    Expr::identifier("x"),
                                    Expr::int_literal("1"),
                                )),
                            },
                        ],
                    ),
                ),
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
    fn test_value_with_conflicting_type_annotations() {
        let expected = vec![canonical::CanonicalizeError::ConflictingTypeAnnotations {
            name: "thing".into(),
            types: vec![
                TypeAnnotation {
                    name: "thing".to_string(),
                    type_variables: vec![],
                    t: Type::identifier("String"),
                },
                TypeAnnotation {
                    name: "thing".to_string(),
                    type_variables: vec![],
                    t: Type::identifier("Int"),
                },
            ],
        }];

        {
            let source: &str = test_source::VALUE_WITH_CONFLICTING_TYPE_ANNOTATIONS;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap_err();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_multi_arg_function_declaration() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new(
                String::from("increment_by_length"),
                Type::lambda(
                    Type::tuple(vec![Type::identifier("Int"), Type::identifier("String")]),
                    Type::identifier("Int"),
                ),
                Expr::lambda(
                    Pattern::identifier("a"),
                    Expr::case(
                        Expr::identifier("a"),
                        vec![
                            Alternative {
                                pattern: Pattern::tuple(vec![
                                    Pattern::int_literal("0"),
                                    Pattern::string_literal(""),
                                ]),
                                matches: Match::Simple(Expr::int_literal("0")),
                            },
                            Alternative {
                                pattern: Pattern::tuple(vec![
                                    Pattern::identifier("x"),
                                    Pattern::identifier("y"),
                                ]),
                                matches: Match::Simple(Expr::bin_op(
                                    "+",
                                    Expr::identifier("x"),
                                    Expr::application(
                                        vec!["String", "length"],
                                        Expr::identifier("y"),
                                    ),
                                )),
                            },
                        ],
                    ),
                ),
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
    fn test_curried_function_declaration() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![canonical::Value::new(
                String::from("apply"),
                Type::lambda(
                    Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                ),
                Expr::lambda(
                    Pattern::identifier("f"),
                    Expr::lambda(
                        Pattern::identifier("value"),
                        Expr::application(vec!["f"], Expr::identifier("value")),
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
            values: vec![canonical::Value::new(
                String::from("apply"),
                Type::lambda(
                    Type::lambda(Type::variable("T3"), Type::variable("T4")),
                    Type::lambda(Type::variable("T3"), Type::variable("T4")),
                ),
                Expr::lambda(
                    Pattern::identifier("f"),
                    Expr::lambda(
                        Pattern::identifier("value"),
                        Expr::application(vec!["f"], Expr::identifier("value")),
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
                    canonical::Value::new(
                        String::from("increment_positive"),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Expr::lambda(
                            Pattern::identifier("num"),
                            Expr::if_else(
                                Expr::application(
                                    vec!["Number", "is_positive?"],
                                    Expr::identifier("num"),
                                ),
                                Expr::bin_op("+", Expr::identifier("num"), Expr::int_literal("1")),
                                Expr::identifier("num"),
                            ),
                        ),
                    ),
                    canonical::Value::new(
                        String::from("decrement_negative"),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Expr::lambda(
                            Pattern::identifier("num"),
                            Expr::if_else(
                                Expr::application(
                                    vec!["Number", "is_negative?"],
                                    Expr::identifier("num"),
                                ),
                                Expr::bin_op("-", Expr::identifier("num"), Expr::int_literal("1")),
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
                values: vec![canonical::Value::new(
                    String::from("increment_or_decrement"),
                    Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    Expr::lambda(
                        Pattern::identifier("num"),
                        Expr::if_else(
                            Expr::application(
                                vec!["Number", "is_positive?"],
                                Expr::identifier("num"),
                            ),
                            Expr::bin_op("+", Expr::identifier("num"), Expr::int_literal("1")),
                            Expr::if_else(
                                Expr::application(
                                    vec!["Number", "is_negative?"],
                                    Expr::identifier("num"),
                                ),
                                Expr::bin_op("-", Expr::identifier("num"), Expr::int_literal("1")),
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
    fn test_call_function_in_module() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![
                canonical::Value::new(
                    String::from("max"),
                    Type::lambda(
                        Type::tuple(vec![Type::identifier("Int"), Type::identifier("Int")]),
                        Type::identifier("Int"),
                    ),
                    Expr::lambda(
                        Pattern::tuple(vec![
                            Pattern::identifier("first"),
                            Pattern::identifier("second"),
                        ]),
                        Expr::if_else(
                            Expr::bin_op(
                                ">",
                                Expr::identifier("first"),
                                Expr::identifier("second"),
                            ),
                            Expr::identifier("first"),
                            Expr::identifier("second"),
                        ),
                    ),
                ),
                canonical::Value::new(
                    String::from("sum_two_largest"),
                    Type::lambda(
                        Type::tuple(vec![
                            Type::identifier("Int"),
                            Type::identifier("Int"),
                            Type::identifier("Int"),
                        ]),
                        Type::identifier("Int"),
                    ),
                    Expr::lambda(
                        Pattern::tuple(vec![
                            Pattern::identifier("num1"),
                            Pattern::identifier("num2"),
                            Pattern::identifier("num3"),
                        ]),
                        Expr::if_else(
                            Expr::bin_op(
                                "==",
                                Expr::identifier("num1"),
                                Expr::application(
                                    vec!["max"],
                                    Expr::tuple(vec![
                                        Expr::identifier("num1"),
                                        Expr::identifier("num2"),
                                    ]),
                                ),
                            ),
                            Expr::bin_op(
                                "+",
                                Expr::identifier("num1"),
                                Expr::application(
                                    vec!["max"],
                                    Expr::tuple(vec![
                                        Expr::identifier("num2"),
                                        Expr::identifier("num3"),
                                    ]),
                                ),
                            ),
                            Expr::bin_op(
                                "+",
                                Expr::identifier("num2"),
                                Expr::application(
                                    vec!["max"],
                                    Expr::tuple(vec![
                                        Expr::identifier("num1"),
                                        Expr::identifier("num3"),
                                    ]),
                                ),
                            ),
                        ),
                    ),
                ),
            ],
            type_aliases: vec![],
        };

        // {
        //     let source: &str = test_source::CALL_FUNCTION_IN_MODULE_WITH_TYPE;
        //
        //     let parsed_module = parse::parser::module(source).unwrap();
        //     let actual = canonicalize(parsed_module).unwrap();
        //
        //     assert_eq!(expected.clone(), actual);
        // }
        // {
        //     let source: &str = test_source::CALL_FUNCTION_IN_MODULE_WITH_ONE_TYPE;
        //
        //     let parsed_module = parse::parser::module(source).unwrap();
        //     let actual = canonicalize(parsed_module).unwrap();
        //
        //     assert_eq!(expected.clone(), actual);
        // }
        {
            let source: &str = test_source::CALL_FUNCTION_IN_MODULE_WITH_NO_TYPE;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap();

            assert_eq!(expected.clone(), actual);
        }
    }

    #[test]
    fn test_multi_property_union_type() {
        let expected = canonical::Module {
            name: String::from("Test"),
            values: vec![],
            type_aliases: vec![canonical::TypeAlias {
                name: String::from("Either"),
                type_variables: vec![String::from("L"), String::from("R")],
                t: Type::union(vec![
                    Type::tuple(vec![Type::atom("Right"), Type::identifier("R")]),
                    Type::tuple(vec![Type::atom("Left"), Type::identifier("L")]),
                ]),
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

    #[test]
    fn test_conflicting_type_alias_definitions() {
        let expected = vec![
            canonical::CanonicalizeError::ConflictingTypeAliasDefinitions {
                name: String::from("Bool"),
                type_aliases: vec![
                    TypeAliasDefinition {
                        name: "Bool".to_string(),
                        type_variables: vec![],
                        t: Type::union(vec![Type::atom("False"), Type::atom("True")]),
                    },
                    TypeAliasDefinition {
                        name: "Bool".to_string(),
                        type_variables: vec![],
                        t: Type::union(vec![Type::atom("True"), Type::atom("False")]),
                    },
                ],
            },
        ];

        {
            let source: &str = test_source::CONFLICTING_TYPE_ALIAS_DEFINITIONS;

            let parsed_module = parse::parser::module(source).unwrap();
            let actual = canonicalize(parsed_module).unwrap_err();

            assert_eq!(expected, actual);
        }
    }
}
