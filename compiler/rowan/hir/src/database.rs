use la_arena::Arena;

use alloy_rowan_ast::IfThenElseExpr;
use alloy_rowan_syntax::SyntaxKind;

use crate::{BinaryOp, Expr, Stmt, UnaryOp};

#[derive(Debug, PartialEq, Default)]
pub struct Database {
    exprs: Arena<Expr>,
}

impl Database {
    pub(crate) fn lower_stmt(&mut self, ast: alloy_rowan_ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            alloy_rowan_ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
                name: ast.name()?.text().into(),
                value: self.lower_expr(ast.value()),
            },
            alloy_rowan_ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
        };

        Some(result)
    }

    pub(crate) fn lower_expr(&mut self, ast: Option<alloy_rowan_ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                alloy_rowan_ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
                alloy_rowan_ast::Expr::IntLiteral(ast) => Expr::IntLiteral { n: ast.parse() },
                alloy_rowan_ast::Expr::FractionalLiteral(ast) => {
                    Expr::FractionalLiteral { n: ast.parse() }
                }
                alloy_rowan_ast::Expr::StringLiteral(ast) => Expr::StringLiteral(ast.parse()),
                alloy_rowan_ast::Expr::CharLiteral(ast) => Expr::CharLiteral(ast.parse()),
                alloy_rowan_ast::Expr::IfThenElseExpr(ast) => self.lower_if_then_else(ast),
                alloy_rowan_ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
                alloy_rowan_ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
                alloy_rowan_ast::Expr::VariableRef(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_binary(&mut self, ast: alloy_rowan_ast::BinaryExpr) -> Expr {
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

    fn lower_if_then_else(&mut self, ast: IfThenElseExpr) -> Expr {
        let cond = self.lower_expr(ast.cond());
        let then = self.lower_expr(ast.then());
        let else_ = self.lower_expr(ast.else_());

        Expr::IfThenElse {
            cond: self.exprs.alloc(cond),
            then: self.exprs.alloc(then),
            else_: self.exprs.alloc(else_),
        }
    }

    fn lower_unary(&mut self, ast: alloy_rowan_ast::UnaryExpr) -> Expr {
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

    fn lower_variable_ref(&mut self, ast: alloy_rowan_ast::VariableRef) -> Expr {
        Expr::VariableRef {
            var: ast.name().unwrap().text().into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::NotNan;

    use alloy_rowan_ast as ast;
    use alloy_rowan_parser as parser;

    use super::*;

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();
        let hir = Database::default().lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
    }

    fn check_expr(input: &str, expected_hir: Expr, expected_database: Database) {
        let root = parse(input);
        let first_stmt = root.stmts().next().unwrap();
        let ast = match first_stmt {
            ast::Stmt::Expr(ast) => ast,
            _ => unreachable!(),
        };
        let mut database = Database::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database);
    }

    #[test]
    fn lower_variable_def_without_name() {
        let root = parse("let = 10");
        let ast = root.stmts().next().unwrap();
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
            Database { exprs },
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
            Database { exprs },
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
    fn lower_unary_expr_without_expr() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::Missing);

        check_expr(
            "-",
            Expr::Unary {
                expr,
                op: UnaryOp::Neg,
            },
            Database { exprs },
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
            Database { exprs },
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
    fn if_else_expr() {
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
            Database { exprs },
        );
    }

    #[test]
    fn if_else_expr_with_missing_exprs() {
        let mut exprs = Arena::new();
        let cond = exprs.alloc(Expr::Missing);
        let then = exprs.alloc(Expr::Missing);
        let else_ = exprs.alloc(Expr::Missing);

        check_expr(
            "if then else",
            Expr::IfThenElse { cond, then, else_ },
            Database { exprs },
        );
    }
}
