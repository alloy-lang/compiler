#[allow(clippy::wildcard_imports)]
use super::*;

ast_union_node!(Expression, kinds: [
    IntLiteral,
    FractionLiteral,
    StringLiteral,
    CharLiteral,
    VariableRef,
    InfixExpr,
    UnitExpr,
    IfThenElseExpr,
    ParenExpr,
    TupleExpr,
    UnaryExpr,
    LambdaExpr,
    FunctionCall,
    MatchExpr
]);

ast_node!(InfixExpr, fields: [lhs, rhs, op]);

impl InfixExpr {
    #[must_use]
    pub fn lhs(&self) -> Option<Expression> {
        children(self).into_iter().nth(0)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<Expression> {
        children(self).into_iter().nth(1)
    }

    #[must_use]
    pub fn op(&self) -> Option<BinaryOp> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(BinaryOp::cast)
    }
}

pub struct BinaryOp(SyntaxToken);

impl BinaryOp {
    #[must_use]
    pub(crate) fn cast(token: SyntaxToken) -> Option<Self> {
        if matches!(
            token.kind(),
            SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Star
                | SyntaxKind::Slash
                | SyntaxKind::OpIdent
        ) {
            Some(Self(token))
        } else {
            None
        }
    }

    pub fn name(&self) -> String {
        self.0.text().to_string()
    }
}

impl fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinaryOp")
            .field("name", &self.name())
            .finish()
    }
}

ast_node!(IfThenElseExpr, fields: [condition, then, else_]);

impl IfThenElseExpr {
    #[must_use]
    pub fn condition(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::IfExpr)?
            .children()
            .find_map(Expression::cast)
    }

    #[must_use]
    pub fn then(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::ThenExpr)?
            .children()
            .find_map(Expression::cast)
    }

    #[must_use]
    pub fn else_(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::ElseExpr)?
            .children()
            .find_map(Expression::cast)
    }
}

ast_node!(UnitExpr, fields: []);
ast_node!(ParenExpr, fields: [expr]);

impl ParenExpr {
    #[must_use]
    pub fn expr(&self) -> Option<Expression> {
        first_child(self)
    }
}

ast_node!(TupleExpr, fields: [exprs]);

impl TupleExpr {
    #[must_use]
    pub fn exprs(&self) -> Vec<Expression> {
        children(self)
    }
}

ast_node!(UnaryExpr, fields: [expr, op]);

impl UnaryExpr {
    #[must_use]
    pub fn expr(&self) -> Option<Expression> {
        first_child(self)
    }

    #[must_use]
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .next()
    }
}

ast_node!(LambdaExpr, fields: [args, body]);

impl LambdaExpr {
    #[must_use]
    pub fn args(&self) -> Vec<Pattern> {
        match_node(self, SyntaxKind::LambdaExprArgList)
            .map(|parent| {
                parent
                    .children()
                    .filter_map(LambdaExprArg::cast)
                    .filter_map(|c| c.pattern())
                    .collect()
            })
            .unwrap_or_default()
    }

    #[must_use]
    pub fn body(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::LambdaExprBody)?
            .children()
            .find_map(Expression::cast)
    }
}

ast_node!(LambdaExprArg, fields: [pattern]);

impl LambdaExprArg {
    fn pattern(&self) -> Option<Pattern> {
        self.0.children_with_tokens().find_map(Pattern::cast)
    }
}

ast_node!(FunctionCall, fields: [target, args]);

impl FunctionCall {
    fn target(&self) -> Option<FunctionCallTarget> {
        first_child(self)
    }

    fn args(&self) -> Vec<FunctionCallArg> {
        match_nodes(self, SyntaxKind::FunctionCallArgList)
            .flat_map(|parent| parent.children().filter_map(FunctionCallArg::cast))
            .collect()
    }
}

ast_node!(FunctionCallTarget, fields: [target]);

impl FunctionCallTarget {
    fn target(&self) -> Option<VariableRef> {
        first_child(self)
    }
}

ast_node!(FunctionCallArg, fields: [expression]);

impl FunctionCallArg {
    fn expression(&self) -> Option<Expression> {
        first_child(self)
    }
}

ast_node!(MatchExpr, fields: [condition, targets]);

impl MatchExpr {
    pub fn condition(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::MatchExprArg)?
            .children()
            .find_map(Expression::cast)
    }

    pub fn targets(&self) -> Vec<MatchTarget> {
        children(self)
    }
}

ast_node!(MatchTarget, fields: [condition, value]);

impl MatchTarget {
    pub fn condition(&self) -> Option<Pattern> {
        match_node(self, SyntaxKind::MatchTargetCondition)?
            .children_with_tokens()
            .find_map(Pattern::cast)
    }

    pub fn value(&self) -> Option<Expression> {
        match_node(self, SyntaxKind::MatchTargetValue)?
            .children()
            .find_map(Expression::cast)
    }
}
