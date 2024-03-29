#[allow(clippy::wildcard_imports)]
use super::*;

ast_union_node!(Expression, kinds: [
    IntLiteral,
    FractionLiteral,
    StringLiteral,
    CharLiteral,
    VariableRef,
    InfixExpr,
    Unit,
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

    #[must_use]
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
        first_matching_child(self, SyntaxKind::IfExpr)
    }

    #[must_use]
    pub fn then(&self) -> Option<Expression> {
        first_matching_child(self, SyntaxKind::ThenExpr)
    }

    #[must_use]
    pub fn else_(&self) -> Option<Expression> {
        first_matching_child(self, SyntaxKind::ElseExpr)
    }
}

ast_node!(ParenExpr, fields: [expression]);

impl ParenExpr {
    #[must_use]
    pub fn expression(&self) -> Option<Expression> {
        first_child(self)
    }
}

ast_node!(TupleExpr, fields: [expressions]);

impl TupleExpr {
    #[must_use]
    pub fn expressions(&self) -> Vec<Expression> {
        children(self)
    }
}

ast_node!(UnaryExpr, fields: [expression, op]);

impl UnaryExpr {
    #[must_use]
    pub fn expression(&self) -> Option<Expression> {
        first_child(self)
    }

    #[must_use]
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .find_map(SyntaxElement::into_token)
    }
}

ast_node!(LambdaExpr, fields: [args, body]);

impl LambdaExpr {
    #[must_use]
    pub fn args(&self) -> Vec<LambdaExprArg> {
        all_matching_children(self, SyntaxKind::LambdaExprArgList)
    }

    #[must_use]
    pub fn body(&self) -> Option<Expression> {
        first_matching_child(self, SyntaxKind::LambdaExprBody)
    }
}

ast_node!(LambdaExprArg, fields: [pattern]);

impl LambdaExprArg {
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        first_child(self)
    }
}

ast_node!(FunctionCall, fields: [target, args]);

impl FunctionCall {
    #[must_use]
    pub fn target(&self) -> Option<VariableRef> {
        first_child(self)
    }

    #[must_use]
    pub fn args(&self) -> Vec<Expression> {
        all_matching_children(self, SyntaxKind::FunctionCallArgList)
    }
}

ast_node!(MatchExpr, fields: [condition, targets]);

impl MatchExpr {
    #[must_use]
    pub fn condition(&self) -> Option<Expression> {
        first_matching_child(self, SyntaxKind::MatchExprArg)
    }

    #[must_use]
    pub fn targets(&self) -> Vec<MatchTarget> {
        children(self)
    }
}

ast_node!(MatchTarget, fields: [condition, value]);

impl MatchTarget {
    #[must_use]
    pub fn condition(&self) -> Option<Pattern> {
        first_matching_child(self, SyntaxKind::MatchTargetCondition)
    }

    #[must_use]
    pub fn value(&self) -> Option<Expression> {
        first_matching_child(self, SyntaxKind::MatchTargetValue)
    }
}
