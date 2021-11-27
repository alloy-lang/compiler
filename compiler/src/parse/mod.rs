use crate::types::Type;
use core::convert::TryFrom;

use itertools::Itertools;
use non_empty_vec::NonEmpty;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Pattern {
    Literal(LiteralData),
    Identifier(String),
    Tuple(NonEmpty<Pattern>),
    Constructor(String, NonEmpty<Pattern>),
    WildCard,
}

impl Pattern {
    pub(crate) fn string_literal<S>(s: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Literal(LiteralData::String(s.into()))
    }

    pub(crate) fn int_literal<S>(int: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Literal(LiteralData::Integral(int.into()))
    }

    pub(crate) fn float_literal<S>(float: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Literal(LiteralData::Fractional(float.into()))
    }

    pub(crate) fn char_literal(c: char) -> Pattern {
        Pattern::Literal(LiteralData::Char(c))
    }

    pub(crate) fn identifier<S>(s: S) -> Pattern
    where
        S: Into<String>,
    {
        Pattern::Identifier(s.into())
    }

    pub(crate) fn tuple<A>(args: A) -> Pattern
    where
        A: Into<Vec<Pattern>>,
    {
        let args = args.into();
        // let n = args.len();
        // let name = tuple_name(n);
        //
        // Pattern::Constructor(name, args)

        Pattern::Tuple(NonEmpty::try_from(args).unwrap())
    }
}

impl From<Pattern> for Vec<Pattern> {
    fn from(p: Pattern) -> Self {
        vec![p]
    }
}

// #[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
// pub(crate) struct Guard {
//     pub predicate: Expr,
//     pub expression: Expr,
// }

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Match {
    // Guards(Vec<Guard>),
    Simple(Expr),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Alternative {
    pub pattern: Pattern,
    pub matches: Match,
    // pub where_bindings: Option<Vec<Binding>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Binding {
    pub name: String,
    pub arguments: Vec<Pattern>,
    pub matches: Match,
    pub where_bindings: Option<Vec<Binding>>,
    pub t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum LiteralData {
    Integral(String),
    Fractional(String),
    String(String),
    Char(char),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) enum Expr {
    Identifier(String),
    Apply(Box<Expr>, Box<Expr>),
    OpApply(Box<Expr>, String, Box<Expr>),
    Literal(LiteralData),
    Lambda(Pattern, Box<Expr>),
    Case(Box<Expr>, NonEmpty<Alternative>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

impl Expr {
    pub(crate) fn string_literal<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Literal(LiteralData::String(s.into()))
    }

    pub(crate) fn int_literal<S>(int: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Literal(LiteralData::Integral(int.into()))
    }

    pub(crate) fn float_literal<S>(float: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Literal(LiteralData::Fractional(float.into()))
    }

    pub(crate) fn char_literal(c: char) -> Expr {
        Expr::Literal(LiteralData::Char(c))
    }

    pub(crate) fn identifier<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        Expr::Identifier(s.into())
    }

    pub(crate) fn lambda<A>(args: A, body: Expr) -> Expr
    where
        A: Into<Vec<Pattern>>,
    {
        args.into()
            .into_iter()
            .rev()
            .fold(body, |body, arg| Expr::Lambda(arg, Box::new(body)))
    }

    pub(crate) fn bin_op<S, E>(op: S, first: E, second: E) -> Expr
    where
        S: Into<String>,
        E: Into<Box<Expr>>,
    {
        Expr::OpApply(first.into(), op.into(), second.into())
    }

    pub(crate) fn case<E>(expr: E, alts: Vec<Alternative>) -> Expr
    where
        E: Into<Box<Expr>>,
    {
        if alts.is_empty() {
            return *expr.into();
        }

        Expr::Case(expr.into(), NonEmpty::try_from(alts).unwrap())
    }

    pub(crate) fn if_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr
    where
        E: Into<Box<Expr>>,
        V1: Into<Box<Expr>>,
        V2: Into<Box<Expr>>,
    {
        Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
    }

    pub(crate) fn application<S, E>(address: Vec<S>, args: E) -> Expr
    where
        S: Into<String>,
        E: Into<Vec<Expr>>,
    {
        let address = address.into_iter().map(|s| s.into()).join("::");
        let func = Expr::Identifier(address);

        args.into()
            .into_iter()
            .fold(func, |func, arg| Expr::Apply(Box::new(func), arg.into()))
    }

    pub(crate) fn tuple<E>(args: E) -> Expr
    where
        E: Into<Vec<Expr>>,
    {
        let args = args.into();
        let n = args.len();
        let name = tuple_name(n);

        Self::application(vec![name], args)
    }

    pub(crate) fn paren<E>(expr: E) -> Expr
    where
        E: Into<Box<Expr>>,
    {
        Expr::Paren(expr.into())
    }
}

fn tuple_name(n: usize) -> String {
    let commas = if n == 0 { 0 } else { n - 1 };

    format!("({})", ",".repeat(commas))
}

impl From<Expr> for Vec<Expr> {
    fn from(e: Expr) -> Self {
        vec![e]
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
    rule traced<T>(e: rule<T>) -> T =
        &(input:$([_]*) {
            #[cfg(feature = "trace")]
            println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
        })
        e:e()? {?
            #[cfg(feature = "trace")]
            println!("[PEG_TRACE_STOP]");
            e.ok_or("")
        }

    // pub(crate)rule module() -> Module = traced(<real_module()>)
    pub(crate)rule module() -> Module = real_module()

    rule real_module() -> Module
        = __ "module" _ name:identifier() __ "where"
          declarations:((__ dec:declaration() _ { dec }) ** "\n") __
          { Module::from_declarations(name, declarations) }

    rule declaration() -> Declaration
        = type_annotation()
        / value_definition()
        / type_alias_definition()

    rule possible_union_type() -> Type
        = ("|")? _ types:((_ t:type_definition() _n_() { t }) ++ "|") { Type::union(types) }
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
        = "|" args:((_ a:pattern() _ { a }) ** ",") "|" _
        "=>" __ definition:expression() _
        { Expr::lambda(args, definition) }

    rule if_else() -> Expr
        = "if" _ e:expression() __
          "then" __ then_body:expression() __
          "else" __ else_body:expression() _
        { Expr::if_else(e, then_body, else_body) }

    rule pattern() -> Pattern
        = "\"" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']* ) "\"" { Pattern::string_literal(n) }
        / "'" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']) "'" { Pattern::char_literal(n.parse().unwrap()) }
        / n:$(['-']?['0'..='9']+ "." ['0'..='9']+) { Pattern::float_literal(n) }
        / n:$(['-']?['0'..='9']+) { Pattern::int_literal(n) }
        / i:identifier() { Pattern::identifier(i) }
        / "(" args:((_ p:pattern() _ { p }) **<2,> ",") ")" { Pattern::tuple(args) }
        / "(" _ p:pattern() _ ")" { p }

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::bin_op("==", a, b) }
        a:@ _ "!=" _ b:(@) { Expr::bin_op("!=", a, b) }
        a:@ _ "<"  _ b:(@) { Expr::bin_op("<", a, b) }
        a:@ _ "<=" _ b:(@) { Expr::bin_op("<=", a, b) }
        a:@ _ ">"  _ b:(@) { Expr::bin_op(">", a, b) }
        a:@ _ ">=" _ b:(@) { Expr::bin_op(">=", a, b) }
        --
        a:@ _ "+" _ b:(@) { Expr::bin_op("+", a, b) }
        a:@ _ "-" _ b:(@) { Expr::bin_op("-", a, b) }
        --
        a:@ _ "*" _ b:(@) { Expr::bin_op("*", a, b) }
        a:@ _ "/" _ b:(@) { Expr::bin_op("/", a, b) }
        --
        "(" args:((_ e:expression() _ { e }) **<2,> ",") ")" { Expr::tuple(args) }
        "(" _ e:expression() _ ")" { Expr::paren(e) }
        address:(identifier() ++ "::") "(" args:((_ e:expression() _ { e }) ** ",") ")" { Expr::application(address, args) }
        i:identifier() { Expr::identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = quiet!{ n:$(['_']?['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*['?' | '\'']?) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = "\"" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']* ) "\"" { Expr::string_literal(n) }
        / "'" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']) "'" { Expr::char_literal(n.parse().unwrap()) }
        / n:$(['-']?['0'..='9']+ "." ['0'..='9']+) { Expr::float_literal(n) }
        / n:$(['-']?['0'..='9']+) { Expr::int_literal(n) }

    rule _n_() =  _ ['\n']? _
    rule __() =  quiet!{[' ' | '\t' | '\n']*}

    rule _() =  quiet!{[' ' | '\t']*}
}
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::parse::{Expr, Pattern, TypeAliasDefinition, TypeAnnotation};
    use crate::test_source;
    use crate::types::Type;

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
        expecteds: Vec<TypeAnnotation>,
        actuals: Vec<TypeAnnotation>,
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
        expecteds: Vec<TypeAliasDefinition>,
        actuals: Vec<TypeAliasDefinition>,
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
                type_annotations: vec![TypeAnnotation {
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
                type_annotations: vec![TypeAnnotation {
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
                type_annotations: vec![TypeAnnotation {
                    name: String::from("increment_positive"),
                    type_variables: vec![],
                    t: Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                }],
                values: vec![
                    parse::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::lambda(Pattern::int_literal("0"), Expr::int_literal("0")),
                    },
                    parse::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::lambda(
                            Pattern::identifier("x"),
                            Expr::bin_op("+", Expr::identifier("x"), Expr::int_literal("1")),
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
                type_annotations: vec![TypeAnnotation {
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
                        definition: Expr::lambda(
                            Pattern::tuple(vec![
                                Pattern::int_literal("0"),
                                Pattern::string_literal(""),
                            ]),
                            Expr::int_literal("0"),
                        ),
                    },
                    parse::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::lambda(
                            Pattern::tuple(vec![
                                Pattern::identifier("x"),
                                Pattern::identifier("y"),
                            ]),
                            Expr::bin_op(
                                "+",
                                Expr::identifier("x"),
                                Expr::application(vec!["String", "length"], Expr::identifier("y")),
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
                type_annotations: vec![TypeAnnotation {
                    name: String::from("apply"),
                    type_variables: vec![],
                    t: Type::lambda(
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    ),
                }],
                values: vec![parse::Value {
                    name: String::from("apply"),
                    definition: Expr::lambda(
                        Pattern::identifier("f"),
                        Expr::lambda(
                            Pattern::identifier("value"),
                            Expr::application(vec!["f"], Expr::identifier("value")),
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
                        definition: Expr::lambda(
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
                    },
                    parse::Value {
                        name: String::from("decrement_negative"),
                        definition: Expr::lambda(
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
                    definition: Expr::lambda(
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
            type_aliases: vec![TypeAliasDefinition {
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
