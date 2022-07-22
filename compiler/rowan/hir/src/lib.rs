use la_arena::Idx;
use ordered_float::NotNan;

use alloy_rowan_ast as ast;

pub use self::database::Database;

mod database;

pub fn lower(ast: ast::Root) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VariableDef { name: String, value: Expr },
    Expr(Expr),
}

type ExprIdx = Idx<Expr>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    IntLiteral {
        /// is `None` if the number is too big to fit in a u64
        n: Option<u64>,
    },
    FractionalLiteral {
        /// is `None` if the number is too big to fit in a f64
        n: Option<NotNan<f64>>,
    },
    StringLiteral(String),
    CharLiteral(Option<char>),
    Unary {
        op: UnaryOp,
        expr: ExprIdx,
    },
    VariableRef {
        var: String,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
}
