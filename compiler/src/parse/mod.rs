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
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    Call(Vec<String>, Vec<Expr>),
    Tuple(Vec<Expr>),
}

impl Expr {
    fn literal<S>(s: S) -> Expr where S: Into<String> {
        Expr::Literal(s.into())
    }

    fn identifier<S>(s: S) -> Expr where S: Into<String> {
        Expr::Identifier(s.into())
    }

    fn assign<S, E>(name: S, expr: E) -> Expr where S: Into<String>, E: Into<Box<Expr>> {
        Expr::Assign(name.into(), expr.into())
    }

    fn function<A, E>(args: A, expr: E) -> Expr where A: Into<Vec<Expr>>, E: Into<Box<Expr>> {
        Expr::Function(args.into(), expr.into())
    }

    fn bin_op<E>(op: BinOp, first: E, second: E) -> Expr where E: Into<Box<Expr>> {
        Expr::BinOp(op, first.into(), second.into())
    }

    fn if_else<E, V1, V2>(expr: E, then_expr: V1, else_expr: V2) -> Expr where E: Into<Box<Expr>>, V1: Into<Vec<Expr>>, V2: Into<Vec<Expr>> {
        Expr::IfElse(expr.into(), then_expr.into(), else_expr.into())
    }

    fn call<E>(address: Vec<&str>, expr: E) -> Expr where E: Into<Vec<Expr>> {
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
    fn tuple<T>(types: Vec<T>) -> Type where T: Into<Box<Type>> {
        let types = types.into_iter()
            .map(|t| t.into())
            .collect::<Vec<_>>();
        Type::Tuple(types)
    }

    fn identifier<S>(s: S) -> Type where S: Into<String> {
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
        types: Vec<Type>,
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
          declarations:((_ dec:declaration() _ {dec}) ** "\n") __
          { Module { name: name, declarations: declarations } }

    rule declaration() -> Declaration
        = type_annotation()
        / value_definition()

    rule type_definition() -> Type
       = _ "(" args:((_ t:type_definition() _ { Box::new(t) }) ++ ",") ")" _ { Type::Tuple(args) }
       / _ t:identifier() _ { Type::Identifier(t) }

    rule type_annotation() -> Declaration
        = __ name:identifier() _ ":" types:(type_definition() ** "->") _
        { Declaration::TypeAnnotation {name: name, types: types } }

    rule value_definition() -> Declaration
        = __ name:identifier() _ "=" _ e:expression() _ { Declaration::Value {name: name, definition: e } }

    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = _ e:expression() _ "\n" { e }

    rule expression() -> Expr
        = if_else()
        / function()
        / binary_op()

    rule function() -> Expr
        = __ "|" args:((_ a:binary_op() _ { a }) ** ",") "|" _
        "=>" _ definition:expression() _
        { Expr::function(args, definition) }

    rule if_else() -> Expr
        = "if" _ e:expression() _ "{" _ "\n"
        then_body:statements() _ "}" _ "else" _ "{" _ "\n"
        else_body:statements() _ "}"
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
        "(" args:((_ e:expression() _ {e}) ++ ",") ")" { Expr::Tuple(args) }
        address:((i:identifier() ++ ".")) "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(address, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
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

    #[test]
    fn test_empty_module() {
        let source: &str = r#"
            module Test
            where



"#;
        assert_eq!(
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
        assert_eq!(
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
        assert_eq!(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("thing"),
                        types: vec![Type::identifier("Int")],
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
        assert_eq!(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("increment_positive"),
                        types: vec![
                            Type::identifier("Int"),
                            Type::identifier("Int"),
                        ],
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
        assert_eq!(
            parse::Module {
                name: String::from("Test"),
                declarations: vec![
                    Declaration::TypeAnnotation {
                        name: String::from("increment_by_length"),
                        types: vec![
                            Type::tuple(vec![
                                Type::identifier("Int"),
                                Type::identifier("String"),
                            ]),
                            Type::identifier("Int"),
                        ],
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

//     #[test]
//     fn test_123() {
//         let source: &str = r#"
//             module Main
//             where
//
//
//
//             fn foo(a: Int, b: Int) -> Int {
//                 c = if a {
//                     if b {
//                         30
//                     } else {
//                         40
//                     }
//                 } else {
//                     50
//                 }
//                 c = c + 2
//             }
// "#;
//         assert_eq!(
//             parse::Module {
//                 name: String::from("Main"),
//                 declarations: vec![
//                     Declaration::Function {
//                         name: String::from("foo"),
//                         args: vec![
//                             (String::from("a"), Type::identifier("Int")),
//                             (String::from("b"), Type::identifier("Int")),
//                         ],
//                         return_type: Type::identifier("Int"),
//                         expressions: vec![
//                             Expr::assign(
//                                 "c",
//                                 Expr::if_else(
//                                     Expr::identifier("a"),
//                                     Expr::if_else(
//                                         Expr::identifier("b"),
//                                         Expr::literal("30"),
//                                         Expr::literal("40"),
//                                     ),
//                                     Expr::literal("50"),
//                                 ),
//                             ),
//                             Expr::assign(
//                                 "c",
//                                 Expr::bin_op(BinOp::Add, Expr::identifier("c"), Expr::literal("2")),
//                             ),
//                         ],
//                     }
//                 ],
//             },
//             parse::parser::module(source).unwrap(),
//         );
//     }
}