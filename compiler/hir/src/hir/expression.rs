#[allow(clippy::wildcard_imports)]
use super::*;

#[allow(clippy::module_name_repetitions)]
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
    Missing,
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
    ctx.add_expression(expression, &ast.syntax())
}

pub(super) fn lower_expression_inner(ctx: &mut LoweringCtx, ast: &ast::Expression) -> Expression {
    match ast {
        ast::Expression::IntLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Expression::IntLiteral(value)
        }
        ast::Expression::FractionLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Expression::FractionLiteral(value)
        }
        ast::Expression::StringLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Expression::StringLiteral(value)
        }
        ast::Expression::CharLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Expression::CharLiteral(value)
        }
        ast::Expression::VariableRef(var) => lower_variable_ref(ctx, var),
        ast::Expression::InfixExpr(e) => lower_infix_expression(ctx, e),
        ast::Expression::Unit(_) => Expression::Unit,
        ast::Expression::IfThenElseExpr(e) => lower_if_then_else_expression(ctx, e),
        ast::Expression::ParenExpr(e) => {
            let Some(inner) = e.expression() else {
                // todo!("validation");
                return Expression::Missing;
            };

            lower_expression_inner(ctx, &inner)
        }
        ast::Expression::TupleExpr(tuple) => {
            let args = tuple
                .expressions()
                .iter()
                .map(|arg| lower_expression(ctx, arg))
                .collect::<Vec<_>>();

            let Ok(args) = NonEmpty::try_from(args) else {
                unreachable!("parsing error")
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
        // todo!("validation");
        return Expression::Missing;
    };
    let Some(op) = e.op() else {
        unreachable!("parsing error")
    };

    let op = match op.text() {
        "-" => UnaryOp::Neg,
        _ => {
            unreachable!("parsing error")
        }
    };

    let inner_id = lower_expression(ctx, &inner);
    Expression::Unary {
        op,
        expression: inner_id,
    }
}

fn lower_variable_ref(ctx: &mut LoweringCtx, var: &ast::VariableRef) -> Expression {
    let Some(path) = var.name() else {
        unreachable!("parsing error")
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        unreachable!("parsing error")
    };

    if !ctx.contains_variable_ref(&name) {
        ctx.error(
            LoweringErrorKind::UnknownReference { path: name.clone() },
            var.range(),
        );
    }

    Expression::VariableRef { name }
}

fn lower_infix_expression(ctx: &mut LoweringCtx, e: &ast::InfixExpr) -> Expression {
    let lhs = match e.lhs() {
        Some(lhs) => lower_expression(ctx, &lhs),
        None => ctx.add_missing_expression(&e.syntax()),
    };
    let rhs = match e.rhs() {
        Some(rhs) => lower_expression(ctx, &rhs),
        None => ctx.add_missing_expression(&e.syntax()),
    };
    let op = match e.op() {
        Some(op) => {
            let op_name = op.name();
            match op_name.as_str() {
                "+" => BinaryOp::Add,
                "-" => BinaryOp::Sub,
                "*" => BinaryOp::Mul,
                "/" => BinaryOp::Div,
                _ => BinaryOp::Custom(Path::from(op_name)),
            }
        }
        None => BinaryOp::Missing,
    };

    Expression::Binary { op, lhs, rhs }
}

fn lower_if_then_else_expression(ctx: &mut LoweringCtx, e: &ast::IfThenElseExpr) -> Expression {
    let condition = match e.condition() {
        Some(condition) => lower_expression(ctx, &condition),
        None => ctx.add_missing_expression(&e.syntax()),
    };
    let then = match e.then() {
        Some(then) => lower_expression(ctx, &then),
        None => ctx.add_missing_expression(&e.syntax()),
    };
    let else_ = match e.else_() {
        Some(else_) => lower_expression(ctx, &else_),
        None => ctx.add_missing_expression(&e.syntax()),
    };

    Expression::IfThenElse {
        condition,
        then,
        else_,
    }
}

fn lower_lambda_expression(ctx: &mut LoweringCtx, e: &ast::LambdaExpr) -> Expression {
    ctx.inside_scope(|ctx| {
        let args = e
            .args()
            .iter()
            .map(|arg| match arg.pattern() {
                Some(arg) => lower_pattern(ctx, &arg),
                None => ctx.add_missing_pattern(&arg.syntax()),
            })
            .collect::<Vec<_>>();
        let body = match e.body() {
            Some(body) => lower_expression(ctx, &body),
            None => ctx.add_missing_expression(&e.syntax()),
        };

        Expression::Lambda { body, args }
    })
}

fn lower_function_call(ctx: &mut LoweringCtx, e: &ast::FunctionCall) -> Expression {
    let Some(target) = e.target() else {
        unreachable!("parsing error")
    };
    let Some(target) = target.name() else {
        unreachable!("parsing error")
    };
    let Ok(target) = Path::try_from(target.segments()) else {
        unreachable!("parsing error")
    };

    let args = e
        .args()
        .iter()
        .map(|arg| lower_expression(ctx, arg))
        .collect::<Vec<_>>();

    Expression::FunctionCall { target, args }
}

fn lower_match_expression(ctx: &mut LoweringCtx, e: &ast::MatchExpr) -> Expression {
    let condition = match e.condition() {
        Some(condition) => lower_expression(ctx, &condition),
        None => ctx.add_missing_expression(&e.syntax()),
    };

    let targets = e
        .targets()
        .iter()
        .map(|a| ctx.inside_scope(|ctx| {
            // map_or_else can't be used here due to a double borrow
            #[allow(clippy::map_unwrap_or)]
            let condition = a
                .condition()
                .map(|c| lower_pattern(ctx, &c))
                .unwrap_or_else(|| ctx.add_missing_pattern(&a.syntax()));
            // map_or_else can't be used here due to a double borrow
            #[allow(clippy::map_unwrap_or)]
            let value = a
                .value()
                .map(|v| lower_expression(ctx, &v))
                .unwrap_or_else(|| ctx.add_missing_expression(&a.syntax()));

            (condition, value)
        }))
        .collect::<Vec<_>>();

    Expression::Match { condition, targets }
}
