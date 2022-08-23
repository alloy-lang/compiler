use la_arena::Idx;
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;

use alloy_rowan_ast as ast;

pub use self::database::Database;

mod database;

#[must_use]
pub fn lower(ast: &ast::Root) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

    (db, stmts)
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VariableDef { name: String, value: Expr },
    Expr(Expr),
}

type PatternIdx = Idx<Pattern>;

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Missing,
    IntLiteral(Option<u64>),
    FractionalLiteral(Option<NotNan<f64>>),
    StringLiteral(String),
    CharLiteral(Option<char>),
    VariableRef { var: String },
}

type ExprIdx = Idx<Expr>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Missing,
    Unit,
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
    IfThenElse {
        cond: ExprIdx,
        then: ExprIdx,
        else_: ExprIdx,
    },
    Unary {
        op: UnaryOp,
        expr: ExprIdx,
    },
    VariableRef {
        var: String,
    },
    Lambda {
        args: Vec<PatternIdx>,
        body: ExprIdx,
    },
    Tuple(NonEmpty<ExprIdx>),
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
