#[allow(clippy::wildcard_imports)]
use super::*;

pub type ExpressionIdx = Idx<Expression>;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Missing,
    /// is `None` if the number is too big to fit in a u64
    IntLiteral(u64),
    /// is `None` if the number is too big to fit in a f64
    FractionLiteral(NotNan<f64>),
    StringLiteral(String),
    CharLiteral(char),
    VariableRef {
        name: Path,
    },
    Binary {
        op: BinaryOp,
        lhs: ExpressionIdx,
        rhs: ExpressionIdx,
    },
    Unit,
    IfThenElse {
        condition: ExpressionIdx,
        then: ExpressionIdx,
        else_: ExpressionIdx,
    },
    Tuple(NonEmpty<ExpressionIdx>),
    Unary {
        op: UnaryOp,
        expression: ExpressionIdx,
    },
    Lambda {
        args: Vec<PatternIdx>,
        body: ExpressionIdx,
    },
    FunctionCall {
        target: Path,
        args: Vec<ExpressionIdx>,
    },
    Match {
        condition: ExpressionIdx,
        targets: Vec<(PatternIdx, ExpressionIdx)>,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Custom(Path),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
}

pub(super) fn lower_expression(ctx: &mut LoweringCtx, ast: &ast::Expression) -> ExpressionIdx {
    let expression = lower_expression_inner(ctx, ast);
    ctx.expression(expression, &ast.syntax())
}

fn lower_expression_inner(ctx: &mut LoweringCtx, ast: &ast::Expression) -> Expression {
    match ast {
        ast::Expression::IntLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Expression::Missing;
            };
            Expression::IntLiteral(value)
        }
        ast::Expression::FractionLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Expression::Missing;
            };
            Expression::FractionLiteral(value)
        }
        ast::Expression::StringLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Expression::Missing;
            };
            Expression::StringLiteral(value)
        }
        ast::Expression::CharLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Expression::Missing;
            };
            Expression::CharLiteral(value)
        }
        ast::Expression::VariableRef(var) => lower_variable_ref(var),
        ast::Expression::InfixExpr(e) => lower_infix_expression(ctx, e),
        ast::Expression::Unit(e) => Expression::Unit,
        ast::Expression::IfThenElseExpr(e) => lower_if_then_else_expression(ctx, e),
        ast::Expression::ParenExpr(e) => {
            todo!("");
        }
        ast::Expression::TupleExpr(tuple) => {
            let args = tuple
                .expressions()
                .iter()
                .map(|arg| lower_expression(ctx, &arg))
                .collect::<Vec<_>>();

            let Ok(args) = NonEmpty::try_from(args) else {
                todo!("validation");
                return Expression::Missing;
            };

            Expression::Tuple(args)
        }
        ast::Expression::UnaryExpr(e) => lower_unary_expression(ctx, e),
        ast::Expression::LambdaExpr(e) => lower_lambda_expression(ctx, e),
        ast::Expression::FunctionCall(e) => lower_function_call(ctx, e),
        ast::Expression::MatchExpr(e) => lower_match_expression(ctx, e),
    }
}

fn lower_unary_expression(ctx: &mut LoweringCtx, e: &ast::UnaryExpr) -> Expression {
    let Some(inner) = e.expression() else {
        todo!("validation");
        return Expression::Missing;
    };
    let Some(op) = e.op() else {
        todo!("validation");
        return Expression::Missing;
    };

    let op = match op.text() {
        "-" => UnaryOp::Neg,
        _ => {
            todo!("validation");
            return Expression::Missing;
        }
    };

    let inner_id = lower_expression(ctx, &inner);
    Expression::Unary {
        op,
        expression: inner_id,
    }
}

fn lower_variable_ref(var: &ast::VariableRef) -> Expression {
    let Some(path) = var.name() else {
        todo!("validation");
        return Expression::Missing;
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        todo!("validation");
        return Expression::Missing;
    };

    Expression::VariableRef { name }
}

fn lower_infix_expression(ctx: &mut LoweringCtx, e: &ast::InfixExpr) -> Expression {
    let Some(lhs) = e.lhs() else {
        todo!("validation");
        return Expression::Missing;
    };
    let lhs = lower_expression(ctx, &lhs);

    let Some(rhs) = e.rhs() else {
        todo!("validation");
        return Expression::Missing;
    };
    let rhs = lower_expression(ctx, &rhs);

    let Some(op) = e.op() else {
        todo!("validation");
        return Expression::Missing;
    };
    let op_name = op.name();
    let op = match op_name.as_str() {
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        _ => BinaryOp::Custom(Path::from(op_name)),
    };

    Expression::Binary { op, lhs, rhs }
}

fn lower_if_then_else_expression(ctx: &mut LoweringCtx, e: &ast::IfThenElseExpr) -> Expression {
    let Some(condition) = e.condition() else {
        todo!("validation");
        return Expression::Missing;
    };
    let condition = lower_expression(ctx, &condition);

    let Some(then) = e.then() else {
        todo!("validation");
        return Expression::Missing;
    };
    let then = lower_expression(ctx, &then);

    let Some(else_) = e.else_() else {
        todo!("validation");
        return Expression::Missing;
    };
    let else_ = lower_expression(ctx, &else_);

    Expression::IfThenElse {
        condition,
        then,
        else_,
    }
}

fn lower_lambda_expression(ctx: &mut LoweringCtx, e: &ast::LambdaExpr) -> Expression {
    let Some(body) = e.body() else {
        todo!("validation");
        return Expression::Missing;
    };
    let args = e
        .args()
        .iter()
        .map(|arg| match arg.pattern() {
            Some(arg) => lower_pattern(ctx, &arg),
            None => {
                todo!("validation");
                return ctx.pattern(Pattern::Missing, &arg.syntax());
            }
        })
        .collect::<Vec<_>>();

    Expression::Lambda {
        body: lower_expression(ctx, &body),
        args,
    }
}

fn lower_function_call(ctx: &mut LoweringCtx, e: &ast::FunctionCall) -> Expression {
    let Some(target) = e.target() else {
        todo!("validation");
        return Expression::Missing;
    };
    let Some(target) = target.name() else {
        todo!("validation");
        return Expression::Missing;
    };
    let Ok(target) = Path::try_from(target.segments()) else {
        todo!("validation");
        return Expression::Missing;
    };

    let args = e
        .args()
        .iter()
        .map(|arg| lower_expression(ctx, &arg))
        .collect::<Vec<_>>();

    Expression::FunctionCall { target, args }
}

fn lower_match_expression(ctx: &mut LoweringCtx, e: &ast::MatchExpr) -> Expression {
    let Some(condition) = e.condition() else {
        todo!("validation");
        return Expression::Missing;
    };
    let condition = lower_expression(ctx, &condition);

    let targets = e
        .targets()
        .iter()
        .map(|a| {
            let condition = a
                .condition()
                .map(|c| lower_pattern(ctx, &c))
                .unwrap_or_else(|| ctx.pattern(Pattern::Missing, &a.syntax()));
            let value = a
                .value()
                .map(|v| lower_expression(ctx, &v))
                .unwrap_or_else(|| ctx.expression(Expression::Missing, &a.syntax()));

            (condition, value)
        })
        .collect::<Vec<_>>();

    Expression::Match {
        condition,
        targets,
    }
}
