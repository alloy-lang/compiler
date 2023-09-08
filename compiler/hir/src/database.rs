use la_arena::Arena;
use non_empty_vec::NonEmpty;

use alloy_syntax::SyntaxKind;

use crate::{
    BinaryOp, Expr, Import, Pattern, Stmt, Trait, TraitMember, Type, TypeAnnotation, UnaryOp,
};

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    exprs: Arena<Expr>,
    patterns: Arena<Pattern>,
}

impl Database {
    pub(crate) fn lower_stmt(&mut self, ast: alloy_ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            alloy_ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
                name: ast.name()?,
                value: self.lower_expr(ast.value()),
            },
            alloy_ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
            alloy_ast::Stmt::Import(ast) => Stmt::Import(Self::lower_import(&ast)),
            alloy_ast::Stmt::Trait(ast) => Stmt::Trait(self.lower_trait(&ast)),
        };

        Some(result)
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<alloy_ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                alloy_ast::Expr::BinaryExpr(ast) => self.lower_binary(&ast),
                alloy_ast::Expr::IntLiteral(ast) => Expr::IntLiteral { n: ast.parse() },
                alloy_ast::Expr::FractionalLiteral(ast) => {
                    Expr::FractionalLiteral { n: ast.parse() }
                }
                alloy_ast::Expr::StringLiteral(ast) => Expr::StringLiteral(ast.parse()),
                alloy_ast::Expr::CharLiteral(ast) => Expr::CharLiteral(ast.parse()),
                alloy_ast::Expr::IfThenElseExpr(ast) => self.lower_if_then_else(&ast),
                alloy_ast::Expr::UnitExpr(_ast) => Expr::Unit,
                alloy_ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
                alloy_ast::Expr::TupleExpr(ast) => self.lower_tuple_expr(ast.exprs()),
                alloy_ast::Expr::UnaryExpr(ast) => self.lower_unary(&ast),
                alloy_ast::Expr::VariableRef(ast) => Self::lower_variable_ref(&ast),
                alloy_ast::Expr::LambdaExpr(ast) => self.lower_lambda(&ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_pattern(ast: alloy_ast::Pattern) -> Pattern {
        match ast {
            alloy_ast::Pattern::IntLiteral(ast) => Pattern::IntLiteral(ast.parse()),
            alloy_ast::Pattern::FractionalLiteral(ast) => Pattern::FractionalLiteral(ast.parse()),
            alloy_ast::Pattern::StringLiteral(ast) => Pattern::StringLiteral(ast.parse()),
            alloy_ast::Pattern::CharLiteral(ast) => Pattern::CharLiteral(ast.parse()),
            alloy_ast::Pattern::VariableRef(ast) => Pattern::VariableRef {
                var: ast.name().unwrap(),
            },
        }
    }

    fn lower_binary(&mut self, ast: &alloy_ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            _ => unreachable!(),
        };

        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        Expr::Binary {
            op,
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
        }
    }

    fn lower_if_then_else(&mut self, ast: &alloy_ast::IfThenElseExpr) -> Expr {
        let cond = self.lower_expr(ast.cond());
        let then = self.lower_expr(ast.then());
        let else_ = self.lower_expr(ast.else_());

        Expr::IfThenElse {
            cond: self.exprs.alloc(cond),
            then: self.exprs.alloc(then),
            else_: self.exprs.alloc(else_),
        }
    }

    fn lower_unary(&mut self, ast: &alloy_ast::UnaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            expr: self.exprs.alloc(expr),
        }
    }

    fn lower_variable_ref(ast: &alloy_ast::VariableRef) -> Expr {
        Expr::VariableRef {
            var: ast.name().unwrap(),
        }
    }

    fn lower_lambda(&mut self, ast: &alloy_ast::LambdaExpr) -> Expr {
        let args = ast
            .args()
            .into_iter()
            .map(|p| {
                let p = Self::lower_pattern(p);
                self.patterns.alloc(p)
            })
            .collect();

        let body = self.lower_expr(ast.body());

        Expr::Lambda {
            args,
            body: self.exprs.alloc(body),
        }
    }

    fn lower_tuple_expr(&mut self, args: Vec<alloy_ast::Expr>) -> Expr {
        let args = args
            .into_iter()
            .map(|arg| {
                let arg = self.lower_expr(Some(arg));
                self.exprs.alloc(arg)
            })
            .collect();

        unsafe { Expr::Tuple(NonEmpty::new_unchecked(args)) }
    }

    fn lower_import(ast: &alloy_ast::Import) -> Import {
        Import {
            path: ast.path(),
            targets: ast.targets(),
        }
    }

    fn lower_trait(&self, ast: &alloy_ast::Trait) -> Trait {
        Trait {
            name: ast.name().unwrap(),
            members: self.lower_trait_members(ast),
        }
    }

    fn lower_trait_members(&self, ast: &alloy_ast::Trait) -> Vec<TraitMember> {
        ast.members()
            .into_iter()
            .map(|tm| match tm {
                alloy_ast::TraitMember::TypeAnnotation(ast) => {
                    TraitMember::TypeAnnotation(TypeAnnotation {
                        name: ast.name().unwrap(),
                        t: self.lower_type(ast.t().unwrap()),
                    })
                }
            })
            .collect()
    }

    fn lower_type(&self, ast: alloy_ast::Type) -> Type {
        match ast {
            alloy_ast::Type::SelfRef => Type::SelfRef,
            alloy_ast::Type::Identifier(ast) => Self::lower_type_identifier(&ast),
            alloy_ast::Type::Lambda(ast) => self.lower_lambda_type(&ast),
        }
    }

    fn lower_type_identifier(ast: &alloy_ast::TypeIdentifier) -> Type {
        Type::Identifier(ast.name().unwrap())
    }

    fn lower_lambda_type(&self, ast: &alloy_ast::LambdaType) -> Type {
        Type::Lambda {
            arg_type: Box::new(self.lower_type(ast.arg_type().unwrap())),
            return_type: Box::new(self.lower_type(ast.return_type().unwrap())),
        }
    }
}

#[cfg(test)]
mod tests {
    use non_empty_vec::NonEmpty;
    use ordered_float::NotNan;

    use crate::{Trait, TraitMember, Type, TypeAnnotation};
    use alloy_ast as ast;
    use alloy_parser as parser;

    use super::*;

    fn parse(input: &str) -> ast::SourceFile {
        let node = parser::parse(input).syntax();
        dbg!(&node);
        ast::SourceFile::cast(node).unwrap()
    }

    #[track_caller]
    fn check_stmt(input: &str, expected_hir: Stmt) {
        let source_file = parse(input);
        let ast = source_file
            .stmts()
            .next()
            .expect("expected at least one statement");
        let hir = Database::default().lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
    }

    #[track_caller]
    fn check_expr(input: &str, expected_hir: Expr, expected_database: Database) {
        let source_file = parse(input);
        let first_stmt = source_file
            .stmts()
            .next()
            .expect("expected at least one expression");
        let ast = match first_stmt {
            ast::Stmt::Expr(ast) => ast,
            _ => unreachable!(),
        };
        let mut database = Database::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database);
    }

    fn from_exprs(exprs: Arena<Expr>) -> Database {
        Database {
            exprs,
            patterns: Default::default(),
        }
    }

    #[test]
    fn lower_variable_def_without_name() {
        let source_file = parse("let = 10");
        let ast = source_file.stmts().next().unwrap();
        assert!(Database::default().lower_stmt(ast).is_none());
    }

    #[test]
    fn lower_variable_def_without_value() {
        check_stmt(
            "let a =",
            Stmt::VariableDef {
                name: "a".into(),
                value: Expr::Missing,
            },
        );
    }

    #[test]
    fn lower_variable_def() {
        check_stmt(
            "let foo = bar",
            Stmt::VariableDef {
                name: "foo".into(),
                value: Expr::VariableRef { var: "bar".into() },
            },
        );
    }

    #[test]
    fn lower_expr_stmt() {
        check_stmt("123", Stmt::Expr(Expr::IntLiteral { n: Some(123) }));
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::IntLiteral { n: Some(10) });
        let rhs = exprs.alloc(Expr::Missing);

        check_expr(
            "10 -",
            Expr::Binary {
                lhs,
                rhs,
                op: BinaryOp::Sub,
            },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::IntLiteral { n: Some(1) });
        let rhs = exprs.alloc(Expr::IntLiteral { n: Some(2) });

        check_expr(
            "1 + 2",
            Expr::Binary {
                lhs,
                rhs,
                op: BinaryOp::Add,
            },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lower_int_literal() {
        check_expr(
            "999",
            Expr::IntLiteral { n: Some(999) },
            Database::default(),
        );
    }

    #[test]
    fn lower_fractional_literal() {
        check_expr(
            "999.19",
            Expr::FractionalLiteral {
                n: Some(NotNan::new(999.19).unwrap()),
            },
            Database::default(),
        );
    }

    #[test]
    fn lower_string_literal() {
        check_expr(
            r#""hello""#,
            Expr::StringLiteral("hello".into()),
            Database::default(),
        );
    }

    #[test]
    fn lower_char_literal() {
        check_expr("'c'", Expr::CharLiteral(Some('c')), Database::default());
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "((((((abc))))))",
            Expr::VariableRef { var: "abc".into() },
            Database::default(),
        );
    }

    #[test]
    fn lower_unit_expr() {
        check_expr("(((((())))))", Expr::Unit, Database::default());
    }

    #[test]
    fn lower_tuple_expr() {
        let mut exprs = Arena::new();
        let foo = exprs.alloc(Expr::VariableRef {
            var: "foo".to_string(),
        });
        let bar = exprs.alloc(Expr::VariableRef {
            var: "bar".to_string(),
        });

        let expected = unsafe { Expr::Tuple(NonEmpty::new_unchecked(vec![foo, bar])) };

        check_expr("(foo, bar)", expected, from_exprs(exprs));
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::Missing);

        check_expr(
            "-",
            Expr::Unary {
                expr,
                op: UnaryOp::Neg,
            },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lower_unary_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(Expr::IntLiteral { n: Some(10) });

        check_expr(
            "-10",
            Expr::Unary {
                expr: ten,
                op: UnaryOp::Neg,
            },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lower_variable_ref() {
        check_expr(
            "foo",
            Expr::VariableRef { var: "foo".into() },
            Database::default(),
        );
    }

    #[test]
    fn if_then_else_expr() {
        let mut exprs = Arena::new();
        let cond = exprs.alloc(Expr::VariableRef { var: "test".into() });
        let ten = exprs.alloc(Expr::IntLiteral { n: Some(10) });
        let five = exprs.alloc(Expr::IntLiteral { n: Some(5) });

        check_expr(
            "if test then 10 else 5",
            Expr::IfThenElse {
                cond,
                then: ten,
                else_: five,
            },
            from_exprs(exprs),
        );
    }

    #[test]
    fn if_then_else_expr_with_missing_exprs() {
        let mut exprs = Arena::new();
        let cond = exprs.alloc(Expr::Missing);
        let then = exprs.alloc(Expr::Missing);
        let else_ = exprs.alloc(Expr::Missing);

        check_expr(
            "if then else",
            Expr::IfThenElse { cond, then, else_ },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lambda_expr_no_args() {
        let mut exprs = Arena::new();
        let body = exprs.alloc(Expr::IntLiteral { n: Some(9) });

        check_expr(
            "|| -> 9",
            Expr::Lambda { args: vec![], body },
            from_exprs(exprs),
        );
    }

    #[test]
    fn lambda_expr_one_arg() {
        let mut exprs = Arena::new();
        let mut patterns = Arena::new();
        let arg1 = patterns.alloc(Pattern::VariableRef {
            var: "arg1".to_string(),
        });
        let body = exprs.alloc(Expr::IntLiteral { n: Some(9) });

        check_expr(
            "|arg1| -> 9",
            Expr::Lambda {
                args: vec![arg1],
                body,
            },
            Database { exprs, patterns },
        );
    }

    #[test]
    fn lambda_expr_multiple_args() {
        let mut exprs = Arena::new();
        let mut patterns = Arena::new();
        let arg1 = patterns.alloc(Pattern::VariableRef {
            var: "arg1".to_string(),
        });
        let arg2 = patterns.alloc(Pattern::VariableRef {
            var: "arg2".to_string(),
        });
        let arg3 = patterns.alloc(Pattern::VariableRef {
            var: "arg3".to_string(),
        });
        let body = exprs.alloc(Expr::IntLiteral { n: Some(9) });

        check_expr(
            "|arg1, arg2, arg3| -> 9",
            Expr::Lambda {
                args: vec![arg1, arg2, arg3],
                body,
            },
            Database { exprs, patterns },
        );
    }

    #[test]
    fn lower_single_import_stmt() {
        check_stmt(
            "import std  ",
            Stmt::Import(Import {
                path: vec![],
                targets: vec!["std".to_string()],
            }),
        );
    }

    #[test]
    fn lower_longer_import_stmt() {
        check_stmt(
            "import std :: functor ::Functor",
            Stmt::Import(Import {
                path: vec!["std".to_string(), "functor".to_string()],
                targets: vec!["Functor".to_string()],
            }),
        );
    }

    #[test]
    fn lower_multi_import_stmt() {
        check_stmt(
            "import std :: functor ::{Functor, map}",
            Stmt::Import(Import {
                path: vec!["std".to_string(), "functor".to_string()],
                targets: vec!["Functor".to_string(), "map".to_string()],
            }),
        );
    }

    #[test]
    fn lower_trait() {
        check_stmt(
            "
            trait TestTrait1 where
                typeof test : self -> String -> Int
            end
            ",
            Stmt::Trait(Trait {
                name: "TestTrait1".to_string(),
                members: vec![TraitMember::TypeAnnotation(TypeAnnotation {
                    name: "test".to_string(),
                    t: Type::Lambda {
                        arg_type: Box::new(Type::SelfRef),
                        return_type: Box::new(Type::Lambda {
                            arg_type: Box::new(Type::Identifier("String".to_string())),
                            return_type: Box::new(Type::Identifier("Int".to_string())),
                        }),
                    },
                })],
            }),
        );
    }
}
