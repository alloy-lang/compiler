#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
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

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Literal(String),
    Identifier(String),
    Assign(String, Box<Expr>),
    Function(Vec<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Vec<String>, Vec<Expr>),
    Tuple(Vec<Expr>),
}

impl Expr {
    pub fn literal<S>(s: S) -> Expr where S: Into<String> {
        Expr::Literal(s.into())
    }

    pub fn identifier<S>(s: S) -> Expr where S: Into<String> {
        Expr::Identifier(s.into())
    }

    fn assign<S, E>(name: S, expr: E) -> Expr where S: Into<String>, E: Into<Box<Expr>> {
        Expr::Assign(name.into(), expr.into())
    }

    fn function<A, E>(args: A, expr: E) -> Expr where A: Into<Vec<Expr>>, E: Into<Box<Expr>> {
        Expr::Function(args.into(), expr.into())
    }

    pub fn bin_op<E>(op: BinOp, first: E, second: E) -> Expr where E: Into<Box<Expr>> {
        Expr::BinOp(op, first.into(), second.into())
    }

    fn if_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr where E: Into<Box<Expr>>, V1: Into<Box<Expr>>, V2: Into<Box<Expr>> {
        Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
    }

    pub fn call<E>(address: Vec<&str>, expr: E) -> Expr where E: Into<Vec<Expr>> {
        let address = address.into_iter::<>()
            .map(|s| String::from(s))
            .collect::<Vec<_>>();

        Expr::Call(address, expr.into())
    }
}

impl From<Expr> for Vec<Expr> {
    fn from(e: Expr) -> Self {
        vec![e]
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Identifier(String),
    Lambda {
        arg_type: Box<Type>,
        return_type: Box<Type>,
    },
    Variable {
        var_name: String,
        var_type: Box<Type>,
    },
    Record {
        properties: Vec<(String, Box<Type>)>,
    },
    Alias {
        type_name: String,
        target: String,
    },
    Union {
        type_name: String,
        head: (String, Box<Type>),
        tail: Vec<(String, Box<Type>)>,
    },
    Tuple(Vec<Box<Type>>),
    Unit,
}

impl Type {
    pub fn tuple<T>(types: Vec<T>) -> Type where T: Into<Box<Type>> {
        let types = types.into_iter()
            .map(|t| t.into())
            .collect::<Vec<_>>();
        Type::Tuple(types)
    }

    fn lambda<T1, T2>(arg_type: T1, return_type: T2) -> Type where T1: Into<Box<Type>>, T2: Into<Box<Type>> {
        Type::Lambda {
            arg_type: arg_type.into(),
            return_type: return_type.into(),
        }
    }

    pub fn identifier<S>(s: S) -> Type where S: Into<String> {
        Type::Identifier(s.into())
    }
}

impl From<Type> for Vec<Type> {
    fn from(t: Type) -> Self {
        vec![t]
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Declaration {
    TypeAnnotation {
        name: String,
        t: Type,
    },
    Value {
        name: String,
        definition: Expr,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub struct Module {
    name: String,
    declarations: Vec<Declaration>,
}

peg::parser!(pub grammar parser() for str {
    pub rule module() -> Module
        = __ "module" _ name:identifier() __ "where"
          declarations:((__ dec:declaration() _ {dec}) ** "\n") __
          { Module { name: name, declarations: declarations } }

    rule declaration() -> Declaration
        = type_annotation()
        / value_definition()

    rule type_definition() -> Type = precedence!{
        arg_type:@ _ "->" _ return_type:(@) { Type::lambda(arg_type, return_type) }
        --
        "(" args:((_ t:type_definition() _ { Box::new(t) }) **<2,> ",") ")" _ { Type::Tuple(args) }
        --
        "(" _ t:type_definition() _ ")" _ { t }
        t:identifier() _ { Type::Identifier(t) }
    }

    rule type_annotation() -> Declaration
        = name:identifier() _ ":" _ t:type_definition() _
        { Declaration::TypeAnnotation {name: name, t: t } }

    rule value_definition() -> Declaration
        = name:identifier() _ "=" _ e:expression() _ { Declaration::Value {name: name, definition: e } }

    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = _ e:expression() _ "\n" { e }

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

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:expression() { Expr::assign(i, e) }

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::BinOp(BinOp::Eq, Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expr::BinOp(BinOp::Ne, Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expr::BinOp(BinOp::Lt, Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expr::BinOp(BinOp::Le, Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Expr::BinOp(BinOp::Gt, Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expr::BinOp(BinOp::Ge, Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expr::BinOp(BinOp::Add, Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expr::BinOp(BinOp::Sub, Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expr::BinOp(BinOp::Mul, Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Expr::BinOp(BinOp::Div, Box::new(a), Box::new(b)) }
        --
        "(" args:((_ e:expression() _ {e}) **<2,> ",") ")" { Expr::Tuple(args) }
        "(" _ e:expression() _ ")" { e }
        address:(identifier() ++ ".") "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(address, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = quiet!{ n:$(['_']?['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*['?']?) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }

    rule __() =  quiet!{[' ' | '\t' | '\n']*}

    rule _() =  quiet!{[' ' | '\t']*}
});

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::parse::{BinOp, Declaration, Expr, Type};

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

    fn assert_declarations(module_name: String, expecteds: Vec<Declaration>, actuals: Vec<Declaration>) {
        let pairs = expecteds.iter()
            .zip(actuals.iter())
            .enumerate()
            .collect::<Vec<_>>();

        for (index, (expected, actual)) in pairs {
            assert_eq!(expected, actual, r#"

Declaration in module '{}' at index {} were not equal.
 expected: {:?}
   actual: {:?}
"#,
                module_name,
                index,
                expected,
                actual,
            );
        }

        assert_eq!(expecteds, actuals);
    }

    fn assert_module(expected: parse::Module, actual: parse::Module) {
        assert_eq!(expected.name, actual.name, r#"

Module names were not equal.
 expected: {}
   actual: {}
"#,
                   expected.name,
                   actual.name,
        );
        assert_declarations(expected.name, expected.declarations, actual.declarations);
    }

    #[test]
    fn test_empty_module() {
        let source: &str = r#"
            module Test
            where



"#;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_value_declaration_no_type() {
        let source: &str = r#"
            module Test
            where

            thing = 0
"#;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::Value {
                        name: String::from("thing"),
                        definition: Expr::literal("0"),
                    }
                ],
            },
            parse::parser::module(source).unwrap(),
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
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("thing"),
                        t: Type::identifier("Int"),
                    },
                    Declaration::Value {
                        name: String::from("thing"),
                        definition: Expr::literal("0"),
                    }
                ],
            },
            parse::parser::module(source).unwrap(),
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
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("increment_positive"),
                        t: Type::lambda(
                            Type::identifier("Int"),
                            Type::identifier("Int"),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(
                            Expr::literal("0"),
                            Expr::literal("0"),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(
                            Expr::identifier("x"),
                            Expr::bin_op(
                                BinOp::Add,
                                Expr::identifier("x"),
                                Expr::literal("1"),
                            ),
                        ),
                    }
                ],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source: &str = r#"
            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, 1)| => 0
            increment_by_length = |(x, y)| => x + String.length(y)
"#;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("increment_by_length"),
                        t: Type::lambda(
                            Type::tuple(vec![
                                Type::identifier("Int"),
                                Type::identifier("String"),
                            ]),
                            Type::identifier("Int"),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::function(
                            Expr::Tuple(vec![Expr::literal("0"), Expr::literal("1")]),
                            Expr::literal("0"),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::function(
                            Expr::Tuple(vec![Expr::identifier("x"), Expr::identifier("y")]),
                            Expr::bin_op(
                                BinOp::Add,
                                Expr::identifier("x"),
                                Expr::call(
                                    vec!["String", "length"],
                                    Expr::identifier("y"),
                                ),
                            ),
                        ),
                    }
                ],
            },
            parse::parser::module(source).unwrap(),
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
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("increment_by_length"),
                        t: Type::lambda(
                            Type::lambda(
                                Type::identifier("Int"),
                                Type::identifier("Int"),
                            ),
                            Type::lambda(
                                Type::identifier("Int"),
                                Type::identifier("Int"),
                            ),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("increment_by_length"),
                        definition: Expr::function(
                            Expr::identifier("f"),
                            Expr::function(
                                Expr::identifier("value"),
                                Expr::call(
                                    vec!["f"],
                                    Expr::identifier("value"),
                                ),
                            ),
                        ),
                    }
                ],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_simple_if_then_else() {
        let source: &str = r#"
            module Test
            where

            increment_positive = |num| =>
              if Number.is_positive?(num)
              then num + 1
              else num
            decrement_negative = |num| =>
              if Number.is_negative?(num)
              then num - 1
              else num
"#;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::Value {
                        name: String::from("increment_positive"),
                        definition: Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Add,
                                    Expr::identifier("num"),
                                    Expr::literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    },
                    Declaration::Value {
                        name: String::from("decrement_negative"),
                        definition: Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Sub,
                                    Expr::identifier("num"),
                                    Expr::literal("1"),
                                ),
                                Expr::identifier("num"),
                            ),
                        ),
                    },
                ],
            },
            parse::parser::module(source).unwrap(),
        );
    }

    #[test]
    fn test_nested_if_then_else() {
        let source: &str = r#"
            module Test
            where

            increment_or_decrement = |num| =>
              if Number.is_positive?(num)
              then num + 1
              else
                if Number.is_negative?(num)
                then num - 1
                else num
"#;
        assert_module(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::Value {
                        name: String::from("increment_or_decrement"),
                        definition: Expr::function(
                            Expr::identifier("num"),
                            Expr::if_else(
                                Expr::call(vec!["Number", "is_positive?"], Expr::identifier("num")),
                                Expr::bin_op(
                                    BinOp::Add,
                                    Expr::identifier("num"),
                                    Expr::literal("1"),
                                ),
                                Expr::if_else(
                                    Expr::call(vec!["Number", "is_negative?"], Expr::identifier("num")),
                                    Expr::bin_op(
                                        BinOp::Sub,
                                        Expr::identifier("num"),
                                        Expr::literal("1"),
                                    ),
                                    Expr::identifier("num"),
                                ),
                            ),
                        ),
                    },
                ],
            },
            parse::parser::module(source).unwrap(),
        );
    }
}