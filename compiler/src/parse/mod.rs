use std::iter::FromIterator;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum BinOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Expr {
    StringLiteral(String),
    FloatLiteral(String),
    IntLiteral(String),
    Identifier(String),
    // TODO 001: function arguments are not an "expresion"
    // they can be a literal value, a variable identifier, or a tuple
    Function(Vec<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Vec<String>, Vec<Expr>),
    Tuple(Vec<Expr>),
    Match(Vec<Expr>),
}

impl Expr {
    pub(crate) fn string_literal<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::StringLiteral(s.into())
    }

    pub(crate) fn int_literal<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::IntLiteral(s.into())
    }

    pub(crate) fn float_literal<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::FloatLiteral(s.into())
    }

    pub(crate) fn identifier<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Identifier(s.into())
    }

    pub(crate) fn function<A, E>(args: A, expr: E) -> Expr
    where
        A: Into<Vec<Expr>>,
        E: Into<Box<Expr>>,
    {
        Expr::Function(args.into(), expr.into())
    }

    pub(crate) fn bin_op<E>(op: BinOp, first: E, second: E) -> Expr
    where
        E: Into<Box<Expr>>,
    {
        Expr::BinOp(op, first.into(), second.into())
    }

    pub(crate) fn if_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr
    where
        E: Into<Box<Expr>>,
        V1: Into<Box<Expr>>,
        V2: Into<Box<Expr>>,
    {
        Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
    }

    pub(crate) fn call<S, E>(address: Vec<S>, expr: E) -> Expr
    where
        S: Into<String>,
        E: Into<Vec<Expr>>,
    {
        let address = address
            .into_iter()
            .map(|s| s.into())
            .collect::<Vec<String>>();

        Expr::Call(address, expr.into())
    }
}

impl From<Expr> for Vec<Expr> {
    fn from(e: Expr) -> Self {
        vec![e]
    }
}

impl FromIterator<Expr> for Expr {
    fn from_iter<T: IntoIterator<Item = Expr>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let (_, upper_bound) = iter.size_hint();

        match upper_bound {
            Some(0) => todo!(),
            // Some(0) => Expr::Unit,
            Some(1) => iter.next().unwrap(),
            _ => Expr::Match(iter.collect()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Type {
    Identifier(String),
    Atom(String),
    // TODO: move type variables out of "parse::Type" if it's not needed for reading source code
    Variable(String),
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
        Type::Variable(s.into())
    }
}

impl From<Type> for Vec<Type> {
    fn from(t: Type) -> Self {
        vec![t]
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeAnnotation {
    pub(crate) name: String,
    pub(crate) type_variables: Vec<String>,
    pub(crate) t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Value {
    pub(crate) name: String,
    pub(crate) definition: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeAliasDefinition {
    pub(crate) name: String,
    pub(crate) type_variables: Vec<String>,
    pub(crate) t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
enum Declaration {
    TypeAnnotation {
        name: String,
        type_variables: Vec<String>,
        t: Type,
    },
    Value {
        name: String,
        definition: Expr,
    },
    TypeAliasDefinition {
        name: String,
        type_variables: Vec<String>,
        t: Type,
    },
}

impl Declaration {
    fn new_type_annotation<S>(name: S, type_variables: Vec<String>, t: Type) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::TypeAnnotation {
            name: name.into(),
            type_variables,
            t,
        }
    }

    fn new_value<S>(name: S, definition: Expr) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::Value {
            name: name.into(),
            definition,
        }
    }

    fn new_type_alias<S>(name: S, type_variables: Vec<String>, t: Type) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::TypeAliasDefinition {
            name: name.into(),
            type_variables,
            t,
        }
    }

    pub(crate) fn categorize(
        declarations: impl IntoIterator<Item = Declaration>,
    ) -> (Vec<TypeAnnotation>, Vec<Value>, Vec<TypeAliasDefinition>) {
        let mut type_annotations: Vec<TypeAnnotation> = Vec::new();
        let mut values: Vec<Value> = Vec::new();
        let mut type_aliases: Vec<TypeAliasDefinition> = Vec::new();

        for declaration in declarations {
            match declaration {
                Declaration::TypeAnnotation {
                    name,
                    type_variables,
                    t,
                } => {
                    type_annotations.push(TypeAnnotation {
                        name,
                        type_variables,
                        t,
                    });
                }
                Declaration::Value { name, definition } => {
                    values.push(Value { name, definition });
                }
                Declaration::TypeAliasDefinition {
                    name,
                    type_variables,
                    t,
                } => {
                    type_aliases.push(TypeAliasDefinition {
                        name,
                        type_variables,
                        t,
                    });
                }
            }
        }

        (type_annotations, values, type_aliases)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Module {
    pub(crate) name: String,
    pub(crate) type_annotations: Vec<TypeAnnotation>,
    pub(crate) values: Vec<Value>,
    pub(crate) type_aliases: Vec<TypeAliasDefinition>,
}

impl Module {
    fn from_declarations<S>(name: S, declarations: Vec<Declaration>) -> Module
    where
        S: Into<String>,
    {
        let (type_annotations, values, type_aliases) = Declaration::categorize(declarations);

        Module {
            name: name.into(),
            type_annotations,
            values,
            type_aliases,
        }
    }
}

peg::parser! {
pub(crate)grammar parser() for str {
    pub(crate)rule module() -> Module
        = __ "module" _ name:identifier() __ "where"
          declarations:((__ dec:declaration() _ { dec }) ** "\n") __
          { Module::from_declarations(name, declarations) }

    rule declaration() -> Declaration
        = type_annotation()
        / value_definition()
        / type_alias_definition()

    rule possible_union_type() -> Type
        = ("|")? _ types:((_ t:type_definition() __ { t }) ++ "|") { Type::Union { types } }
        / t:type_definition() { t }

    rule type_definition() -> Type = precedence!{
        arg_type:@ _ "->" _ return_type:(@) { Type::lambda(arg_type, return_type) }
        --
        "(" args:((_ t:type_definition() _ { t }) **<2,> ",") ")" _ { Type::tuple(args) }
        --
        "(" _ t:type_definition() _ ")" _ { t }
        ":" t:identifier() _ { Type::atom(t) }
        t:identifier() _ { Type::identifier(t) }
    }

    rule type_annotation() -> Declaration
        = name:identifier() _ ":" _ t:type_definition() _
        { Declaration::new_type_annotation(name, Vec::new(), t) }

    rule value_definition() -> Declaration
        = name:identifier() _ "=" _ e:expression() _ { Declaration::new_value(name, e) }

    rule type_alias_definition() -> Declaration
        = quiet!{ "data" _ name:identifier() _
        "<" _ type_variables:((_ t:identifier() _ { t }) ** ",") _ ">" _ "=" __
        t:possible_union_type()
        { Declaration::new_type_alias(name, type_variables, t) } }
        / expected!("type alias")

    rule expression() -> Expr
        = if_else()
        / function()
        / binary_op()

    rule function() -> Expr
        = "|" args:((_ a:binary_op() _ { a }) ** ",") "|" _
        "=>" __ definition:expression() _
        { Expr::function(args, definition) }

    rule if_else() -> Expr
        = "if" _ e:expression() __
          "then" __ then_body:expression() __
          "else" __ else_body:expression() _
        { Expr::if_else(e, then_body, else_body) }

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::bin_op(BinOp::Eq, a, b) }
        a:@ _ "!=" _ b:(@) { Expr::bin_op(BinOp::Ne, a, b) }
        a:@ _ "<"  _ b:(@) { Expr::bin_op(BinOp::Lt, a, b) }
        a:@ _ "<=" _ b:(@) { Expr::bin_op(BinOp::Le, a, b) }
        a:@ _ ">"  _ b:(@) { Expr::bin_op(BinOp::Gt, a, b) }
        a:@ _ ">=" _ b:(@) { Expr::bin_op(BinOp::Ge, a, b) }
        --
        a:@ _ "+" _ b:(@) { Expr::bin_op(BinOp::Add, a, b) }
        a:@ _ "-" _ b:(@) { Expr::bin_op(BinOp::Sub, a, b) }
        --
        a:@ _ "*" _ b:(@) { Expr::bin_op(BinOp::Mul, a, b) }
        a:@ _ "/" _ b:(@) { Expr::bin_op(BinOp::Div, a, b) }
        --
        "(" args:((_ e:expression() _ { e }) **<2,> ",") ")" { Expr::Tuple(args) }
        "(" _ e:expression() _ ")" { e }
        address:(identifier() ++ "::") "(" args:((_ e:expression() _ { e }) ** ",") ")" { Expr::call(address, args) }
        i:identifier() { Expr::identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = quiet!{ n:$(['_']?['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*['?']?) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = "\"" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']* ) "\"" { Expr::string_literal(n) }
        / n:$(['0'..='9']+ "." ['0'..='9']+) { Expr::float_literal(n) }
        / n:$(['0'..='9']+) { Expr::int_literal(n) }

    rule __() =  quiet!{[' ' | '\t' | '\n']*}

    rule _() =  quiet!{[' ' | '\t']*}
}
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::parse::{BinOp, Expr, Type};
    use crate::test_source;

    macro_rules! assert_eq {
        ($expected:expr, $actual:expr) => ({
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

    fn assert_type_annotations(
        module_name: &String,
        expecteds: Vec<parse::TypeAnnotation>,
        actuals: Vec<parse::TypeAnnotation>,
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

TypeAnnotation in module '{}' at index {} were not equal.
 expected: {:?}
   actual: {:?}
"#,
                module_name, index, expected, actual,
            );
        }

        assert_eq!(expecteds, actuals);
    }

    fn assert_values(
        module_name: &String,
        expecteds: Vec<parse::Value>,
        actuals: Vec<parse::Value>,
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

Value in module '{}' at index {} were not equal.
 expected: {:?}
   actual: {:?}
"#,
                module_name, index, expected, actual,
            );
        }

        assert_eq!(expecteds, actuals);
    }

    fn assert_type_aliases(
        module_name: &String,
        expecteds: Vec<parse::TypeAliasDefinition>,
        actuals: Vec<parse::TypeAliasDefinition>,
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

TypeAlias in module '{}' at index {} were not equal.
 expected: {:?}
   actual: {:?}
"#,
                module_name, index, expected, actual,
            );
        }

        assert_eq!(expecteds, actuals);
    }

    fn assert_module(expected: parse::Module, actual: parse::Module) {
        assert_eq!(
            expected.name, actual.name,
            r#"

Module names were not equal.
 expected: {}
   actual: {}
"#,
            expected.name, actual.name,
        );

        assert_type_annotations(
            &expected.name,
            expected.type_annotations,
            actual.type_annotations,
        );
        assert_values(&expected.name, expected.values, actual.values);
        assert_type_aliases(&expected.name, expected.type_aliases, actual.type_aliases);
    }

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                type_aliases: vec![],
                values: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_int_value_declaration_no_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                values: vec![parse::Value {
                    name: String::from("thing"),
                    definition: Expr::int_literal("0"),
                }],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_int_value_declaration_with_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![parse::TypeAnnotation {
                    name: String::from("thing"),
                    type_variables: vec![],
                    t: Type::identifier("Int"),
                }],
                values: vec![parse::Value {
                    name: String::from("thing"),
                    definition: Expr::int_literal("0"),
                }],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_float_value_declaration_no_type() {
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                values: vec![parse::Value {
                    name: String::from("thing"),
                    definition: Expr::float_literal("0.1"),
                }],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_float_value_declaration_with_type() {
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![parse::TypeAnnotation {
                    name: String::from("thing"),
                    type_variables: vec![],
                    t: Type::identifier("Float"),
                }],
                values: vec![parse::Value {
                    name: String::from("thing"),
                    definition: Expr::float_literal("0.1"),
                }],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_single_arg_function_declaration_with_type() {
        let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![parse::TypeAnnotation {
                    name: String::from("increment_positive"),
                    type_variables: vec![],
                    t: Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                }],
                values: vec![
                    parse::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(Expr::int_literal("0"), Expr::int_literal("0")),
                    },
                    parse::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(
                            Expr::identifier("x"),
                            Expr::bin_op(BinOp::Add, Expr::identifier("x"), Expr::int_literal("1")),
                        ),
                    },
                ],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source: &str = test_source::MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![parse::TypeAnnotation {
                    name: String::from("increment_by_length"),
                    type_variables: vec![],
                    t: Type::lambda(
                        Type::tuple(vec![Type::identifier("Int"), Type::identifier("String")]),
                        Type::identifier("Int"),
                    ),
                }],
                values: vec![
                    parse::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::function(
                            Expr::Tuple(vec![Expr::int_literal("0"), Expr::string_literal("")]),
                            Expr::int_literal("0"),
                        ),
                    },
                    parse::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::function(
                            Expr::Tuple(vec![Expr::identifier("x"), Expr::identifier("y")]),
                            Expr::bin_op(
                                BinOp::Add,
                                Expr::identifier("x"),
                                Expr::call(vec!["String", "length"], Expr::identifier("y")),
                            ),
                        ),
                    },
                ],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let source: &str = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![parse::TypeAnnotation {
                    name: String::from("apply"),
                    type_variables: vec![],
                    t: Type::lambda(
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    ),
                }],
                values: vec![parse::Value {
                    name: String::from("apply"),
                    definition: Expr::function(
                        Expr::identifier("f"),
                        Expr::function(
                            Expr::identifier("value"),
                            Expr::call(vec!["f"], Expr::identifier("value")),
                        ),
                    ),
                }],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_simple_if_then_else() {
        let source: &str = test_source::SIMPLE_IF_THEN_ELSE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                values: vec![
                    parse::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Add,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    },
                    parse::Value {
                        name: String::from("decrement_negative"),
                        definition: Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Sub,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    },
                ],
                type_aliases: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_nested_if_then_else() {
        let source: &str = test_source::NESTED_IF_THEN_ELSE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                type_aliases: vec![],
                values: vec![parse::Value {
                    name: String::from("increment_or_decrement"),
                    definition: Expr::function(
                        Expr::identifier("num"),
                        Expr::if_else(
                            Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                            Expr::bin_op(
                                BinOp::Add,
                                Expr::identifier("num"),
                                Expr::int_literal("1"),
                            ),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Sub,
                                    Expr::identifier("num"),
                                    Expr::int_literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    ),
                }],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_multi_property_union_type() {
        let expected = parse::Module {
            name: String::from("Test"),
            type_annotations: vec![],
            values: vec![],
            type_aliases: vec![parse::TypeAliasDefinition {
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

            assert_module(expected.clone(), parse::parser::module(source).unwrap());
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_2;

            assert_module(expected.clone(), parse::parser::module(source).unwrap());
        }
        {
            let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_3;

            assert_module(expected, parse::parser::module(source).unwrap());
        }
    }
}
