use crate::types::Type;
use crate::types::TypeVariable;

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
        let address = address.into_iter().map(Into::into).join("::");
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
pub(crate) struct TypeAnnotationDefinition {
    pub(crate) name: String,
    pub(crate) type_variables: Vec<TypeVariable>,
    pub(crate) t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct ValueDefinition {
    pub(crate) name: String,
    pub(crate) definition: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TypeAliasDefinition {
    pub(crate) name: String,
    pub(crate) type_variables: Vec<TypeVariable>,
    pub(crate) t: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct TraitDefinition {
    pub(crate) name: String,
    pub(crate) type_variables: Vec<TypeVariable>,
    pub(crate) type_annotations: Vec<TypeAnnotationDefinition>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
enum Declaration {
    TypeAnnotation(TypeAnnotationDefinition),
    Value(ValueDefinition),
    TypeAlias(TypeAliasDefinition),
    Trait(TraitDefinition),
}

impl Declaration {
    fn new_type_annotation<S>(
        name: S,
        type_variables: Vec<TypeVariable>,
        t: Type,
    ) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::TypeAnnotation(TypeAnnotationDefinition {
            name: name.into(),
            type_variables,
            t,
        })
    }

    fn new_value<S>(name: S, definition: Expr) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::Value(ValueDefinition {
            name: name.into(),
            definition,
        })
    }

    fn new_type_alias<S>(name: S, type_variables: Vec<TypeVariable>, t: Type) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::TypeAlias(TypeAliasDefinition {
            name: name.into(),
            type_variables,
            t,
        })
    }

    fn new_trait<S>(
        name: S,
        type_variables: Vec<TypeVariable>,
        type_annotations: Vec<TypeAnnotationDefinition>,
    ) -> Declaration
    where
        S: Into<String>,
    {
        Declaration::Trait(TraitDefinition {
            name: name.into(),
            type_variables,
            type_annotations,
        })
    }

    pub(crate) fn categorize(
        declarations: impl IntoIterator<Item = Declaration>,
    ) -> (
        Vec<TypeAnnotationDefinition>,
        Vec<ValueDefinition>,
        Vec<TypeAliasDefinition>,
        Vec<TraitDefinition>,
    ) {
        let mut type_annotations: Vec<TypeAnnotationDefinition> = Vec::new();
        let mut values: Vec<ValueDefinition> = Vec::new();
        let mut type_aliases: Vec<TypeAliasDefinition> = Vec::new();
        let mut traits: Vec<TraitDefinition> = Vec::new();

        for declaration in declarations {
            match declaration {
                Declaration::TypeAnnotation(ta) => type_annotations.push(ta),
                Declaration::Value(v) => values.push(v),
                Declaration::TypeAlias(tad) => type_aliases.push(tad),
                Declaration::Trait(td) => traits.push(td),
            }
        }

        (type_annotations, values, type_aliases, traits)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub(crate) struct Module {
    pub(crate) name: String,
    pub(crate) type_annotations: Vec<TypeAnnotationDefinition>,
    pub(crate) values: Vec<ValueDefinition>,
    pub(crate) type_aliases: Vec<TypeAliasDefinition>,
    pub(crate) traits: Vec<TraitDefinition>,
}

impl Module {
    fn from_declarations<S>(name: S, declarations: Vec<Declaration>) -> Module
    where
        S: Into<String>,
    {
        let (type_annotations, values, type_aliases, traits) =
            Declaration::categorize(declarations);

        Module {
            name: name.into(),
            type_annotations,
            values,
            type_aliases,
            traits,
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
        = t:trait_definition() { Declaration::Trait(t) }
        / t:type_annotation() { Declaration::TypeAnnotation(t) }
        / t:value_definition() { Declaration::Value(t) }
        / t:type_alias_definition() { Declaration::TypeAlias(t) }

    rule possible_union_type() -> Type
        = ("|")? _ types:((_ t:type_definition() _n_() { t }) ++ "|") { Type::union(types) }
        / t:type_definition() { t }

    rule type_definition() -> Type = precedence!{
        arg_type:@ _ "->" _ return_type:(@) { Type::lambda(arg_type, return_type) }
        --
        "(" args:((_ t:type_definition() _ { t }) **<2,> ",") ")" _ { Type::tuple(args) }
        --
        t:identifier() _ "<" binds:((t:type_definition() { t }) ** ",") ">" _ { Type::bound(Type::identifier(t), binds) }
        --
        "(" _ t:type_definition() _ ")" _ { t }
        ":" t:identifier() _ { Type::atom(t) }
        t:identifier() _ { Type::identifier(t) }
    }

    rule type_annotation() -> TypeAnnotationDefinition
        = quiet!{ comments() _ name:identifier() type_vars:type_variables() _ ":" _ t:type_definition() _
            type_variables:(
                "where" _ "\n" tvar_bounds:type_variable_bounds() { tvar_bounds }
                / _ { Vec::new() }
            )
            { TypeAnnotationDefinition { name, type_variables, t } }
        }
        / expected!("type annotation")

    rule value_definition() -> ValueDefinition
        = quiet!{ comments()
        _ name:identifier() _ "=" _ e:expression() _ { ValueDefinition { name, definition: e } } }
        / expected!("value definition")

    rule trait_definition() -> TraitDefinition
        = comments()
        "trait" _ name:identifier() type_vars:type_variables() _ "where" _ "\n"
        type_variables:type_variable_bounds()
        type_annotations:((tanno:type_annotation() { tanno })*)
        { TraitDefinition { name, type_variables, type_annotations } }

    // rule type_variable_bounds() -> Vec<TypeVariableBound>
    rule type_variable_bounds() -> Vec<TypeVariable>
        = _ tvar_bounds:((b:type_variable_bound() { b })*) _ { tvar_bounds }

    rule type_variable_bound() -> TypeVariable
        = quiet!{ comments()
        _ "typevar" _ tvar_id:identifier() _ { TypeVariable::new_type(tvar_id) } }
        / expected!("type variable bound")

    rule type_alias_definition() -> TypeAliasDefinition
        = quiet!{ "data" _ name:identifier() type_variables:type_variables() "=" __
        t:possible_union_type()
        { TypeAliasDefinition { name, type_variables, t } } }
        / expected!("type alias")

    rule type_variables() -> Vec<TypeVariable>
        = _ "<" tvars:((t:type_variable() { t }) ** ",") ">" _ { tvars }
        / _ { Vec::new() }

    rule type_variable() -> TypeVariable = precedence!{
        _ t:identifier() tvars:type_variables() { TypeVariable::new_type_function(t, tvars.len()) }
        --
        _ t:identifier() _ { TypeVariable::new_type(t) }
    }

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
        = quiet!{
            n:$("_") { n.to_owned() }
            / n:$(['_']?['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*['?' | '\'']?) { n.to_owned() }
            / n:$(['(']['|' | '<' | '>']+[')']) { n.to_owned() }
        }
        / expected!("identifier")

    rule literal() -> Expr
        = "\"" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']* ) "\"" { Expr::string_literal(n) }
        / "'" n:$(['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/']) "'" { Expr::char_literal(n.parse().unwrap()) }
        / n:$(['-']?['0'..='9']+ "." ['0'..='9']+) { Expr::float_literal(n) }
        / n:$(['-']?['0'..='9']+) { Expr::int_literal(n) }

    #[cache]
    rule comments() -> Vec<String>
        = __ comments:((c:comment() { c }) *) { comments }

    rule comment() -> String
        = _ "//"  s:$([^'\n']*) "\n"+ { s.to_owned() }
        / _ "///" s:$([^'\n']*) "\n"+ { s.to_owned() }

    rule _n_() =  _ ['\n']? _
    rule __() =  quiet!{[' ' | '\t' | '\n']*}

    rule _() =  quiet!{[' ' | '\t']*}
}
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        Expr, Pattern, TraitDefinition, TypeAliasDefinition, TypeAnnotationDefinition,
    };
    use crate::types::Type;
    use crate::types::TypeVariable;
    use crate::parse;
    use std::fs;

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
        expecteds: Vec<TypeAnnotationDefinition>,
        actuals: Vec<TypeAnnotationDefinition>,
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
        expecteds: Vec<parse::ValueDefinition>,
        actuals: Vec<parse::ValueDefinition>,
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

    fn assert_traits(
        module_name: &String,
        expecteds: Vec<TraitDefinition>,
        actuals: Vec<TraitDefinition>,
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

Trait in module '{}' at index {} were not equal.
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
        assert_traits(&expected.name, expected.traits, actual.traits);
    }

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                type_annotations: vec![],
                type_aliases: vec![],
                traits: vec![],
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
                values: vec![parse::ValueDefinition {
                    name: String::from("thing"),
                    definition: Expr::int_literal("0"),
                }],
                type_aliases: vec![],
                traits: vec![],
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
                type_annotations: vec![TypeAnnotationDefinition {
                    name: String::from("thing"),
                    type_variables: vec![],
                    t: Type::identifier("Int"),
                }],
                values: vec![parse::ValueDefinition {
                    name: String::from("thing"),
                    definition: Expr::int_literal("0"),
                }],
                type_aliases: vec![],
                traits: vec![],
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
                values: vec![parse::ValueDefinition {
                    name: String::from("thing"),
                    definition: Expr::float_literal("0.1"),
                }],
                type_aliases: vec![],
                traits: vec![],
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
                type_annotations: vec![TypeAnnotationDefinition {
                    name: String::from("thing"),
                    type_variables: vec![],
                    t: Type::identifier("Float"),
                }],
                values: vec![parse::ValueDefinition {
                    name: String::from("thing"),
                    definition: Expr::float_literal("0.1"),
                }],
                type_aliases: vec![],
                traits: vec![],
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
                type_annotations: vec![TypeAnnotationDefinition {
                    name: String::from("increment_positive"),
                    type_variables: vec![],
                    t: Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                }],
                values: vec![
                    parse::ValueDefinition {
                        name: String::from("increment_positive"),
                        definition: Expr::lambda(Pattern::int_literal("0"), Expr::int_literal("0")),
                    },
                    parse::ValueDefinition {
                        name: String::from("increment_positive"),
                        definition: Expr::lambda(
                            Pattern::identifier("x"),
                            Expr::bin_op("+", Expr::identifier("x"), Expr::int_literal("1")),
                        ),
                    },
                ],
                type_aliases: vec![],
                traits: vec![],
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
                type_annotations: vec![TypeAnnotationDefinition {
                    name: String::from("increment_by_length"),
                    type_variables: vec![],
                    t: Type::lambda(
                        Type::tuple(vec![Type::identifier("Int"), Type::identifier("String")]),
                        Type::identifier("Int"),
                    ),
                }],
                values: vec![
                    parse::ValueDefinition {
                        name: String::from("increment_by_length"),
                        definition: Expr::lambda(
                            Pattern::tuple(vec![
                                Pattern::int_literal("0"),
                                Pattern::string_literal(""),
                            ]),
                            Expr::int_literal("0"),
                        ),
                    },
                    parse::ValueDefinition {
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
                traits: vec![],
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
                type_annotations: vec![TypeAnnotationDefinition {
                    name: String::from("apply"),
                    type_variables: vec![],
                    t: Type::lambda(
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                        Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                    ),
                }],
                values: vec![parse::ValueDefinition {
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
                traits: vec![],
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
                    parse::ValueDefinition {
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
                    parse::ValueDefinition {
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
                traits: vec![],
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
                traits: vec![],
                values: vec![parse::ValueDefinition {
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
                type_variables: vec![TypeVariable::new_type("L"), TypeVariable::new_type("R")],
                t: Type::union(vec![
                    Type::tuple(vec![Type::atom("Right"), Type::identifier("R")]),
                    Type::tuple(vec![Type::atom("Left"), Type::identifier("L")]),
                ]),
            }],
            traits: vec![],
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

    #[test]
    fn test_trait_with_no_typevar() {
        let expected = parse::Module {
            name: String::from("Test"),
            type_annotations: vec![],
            values: vec![],
            type_aliases: vec![],
            traits: vec![TraitDefinition {
                name: "Simple".to_string(),
                type_variables: vec![],
                type_annotations: vec![TypeAnnotationDefinition {
                    name: "simple".to_string(),
                    type_variables: vec![],
                    t: Type::lambda(Type::identifier("Int"), Type::identifier("Int")),
                }],
            }],
        };

        {
            let source: &str = r#"
            module Test
            where

            trait Simple where
              simple: Int -> Int
"#;

            assert_module(expected.clone(), parse::parser::module(source).unwrap());
        }
    }

    #[test]
    fn test_trait_with_multiple_typevars() {
        let expected = parse::Module {
            name: String::from("Test"),
            type_annotations: vec![],
            values: vec![],
            type_aliases: vec![],
            traits: vec![TraitDefinition {
                name: "MultiVar".to_string(),
                type_variables: vec![TypeVariable::new_type("a"), TypeVariable::new_type("b")],
                type_annotations: vec![],
            }],
        };

        {
            let source: &str = r#"
            module Test
            where

            trait MultiVar<a, b> where
              typevar a
              typevar b
"#;

            assert_module(expected.clone(), parse::parser::module(source).unwrap());
        }
    }

    #[test]
    fn test_bound_types() {
        let expected = parse::Module {
            name: String::from("Test"),
            type_annotations: vec![TypeAnnotationDefinition {
                name: "convert".to_string(),
                type_variables: vec![],
                t: Type::lambda(
                    Type::bound(Type::identifier("List"), vec![Type::identifier("String")]),
                    Type::bound(Type::identifier("List"), vec![Type::identifier("Int")]),
                ),
            }],
            values: vec![],
            type_aliases: vec![],
            traits: vec![],
        };

        {
            let source: &str = r#"
            module Test
            where

            convert : List<String> -> List<Int>
"#;

            assert_module(expected.clone(), parse::parser::module(source).unwrap());
        }
    }
}
