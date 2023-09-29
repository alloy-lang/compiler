#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug)]
pub enum Expression {
    IntLiteral(IntLiteral),
    FractionLiteral(FractionLiteral),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    VariableRef(VariableRef),
    BinaryExpr(BinaryExpr),
    UnitExpr(UnitExpr),
    IfThenElseExpr(IfThenElseExpr),
    ParenExpr(ParenExpr),
    TupleExpr(TupleExpr),
    UnaryExpr(UnaryExpr),
    LambdaExpr(LambdaExpr),
    FunctionCall(FunctionCall),
    MatchExpr(MatchExpr),
}

impl Expression {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral::cast(node)?),
            SyntaxKind::FractionLiteral => Self::FractionLiteral(FractionLiteral::cast(node)?),
            SyntaxKind::StringLiteral => Self::StringLiteral(StringLiteral::cast(node)?),
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral::cast(node)?),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef::cast(node)?),
            SyntaxKind::InfixExpr => Self::BinaryExpr(BinaryExpr::cast(node)?),
            SyntaxKind::UnitExpr => Self::UnitExpr(UnitExpr::cast(node)?),
            SyntaxKind::IfThenElseExpr => Self::IfThenElseExpr(IfThenElseExpr::cast(node)?),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr::cast(node)?),
            SyntaxKind::TupleExpr => Self::TupleExpr(TupleExpr::cast(node)?),
            SyntaxKind::PrefixExpr => Self::UnaryExpr(UnaryExpr::cast(node)?),
            SyntaxKind::LambdaExprDef => Self::LambdaExpr(LambdaExpr::cast(node)?),
            SyntaxKind::FunctionCall => Self::FunctionCall(FunctionCall::cast(node)?),
            SyntaxKind::MatchExpr => Self::MatchExpr(MatchExpr::cast(node)?),
            _ => return None,
        };

        Some(result)
    }
}

pub struct BinaryExpr(SyntaxNode);

impl BinaryExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::InfixExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn lhs(&self) -> Option<Expression> {
        self.0.children().find_map(Expression::cast)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<Expression> {
        self.0.children().filter_map(Expression::cast).nth(1)
    }

    #[must_use]
    pub fn op(&self) -> Option<BinaryOp> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(BinaryOp::cast)
    }
}

impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinaryExpr")
            .field("lhs", &self.lhs())
            .field("rhs", &self.rhs())
            .field("op", &self.op())
            .finish()
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

pub struct IfThenElseExpr(SyntaxNode);

impl IfThenElseExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IfThenElseExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn cond(&self) -> Option<Expression> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::IfExpr))
            .and_then(|parent| parent.children().find_map(Expression::cast))
    }

    #[must_use]
    pub fn then(&self) -> Option<Expression> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::ThenExpr))
            .and_then(|parent| parent.children().find_map(Expression::cast))
    }

    #[must_use]
    pub fn else_(&self) -> Option<Expression> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::ElseExpr))
            .and_then(|parent| parent.children().find_map(Expression::cast))
    }
}

impl fmt::Debug for IfThenElseExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IfThenElseExpr")
            .field("cond", &self.cond())
            .field("then", &self.then())
            .field("else_", &self.else_())
            .finish()
    }
}

pub struct UnitExpr(SyntaxNode);

impl UnitExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::UnitExpr {
            Some(Self(node))
        } else {
            None
        }
    }
}

impl fmt::Debug for UnitExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnitExpr").finish()
    }
}

pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ParenExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn expr(&self) -> Option<Expression> {
        self.0.children().find_map(Expression::cast)
    }
}

impl fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParenExpr")
            .field("expr", &self.expr())
            .finish()
    }
}

pub struct TupleExpr(SyntaxNode);

impl TupleExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TupleExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn exprs(&self) -> impl Iterator<Item = Expression> {
        self.0.children().filter_map(Expression::cast)
    }
}

impl fmt::Debug for TupleExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TupleExpr")
            .field("exprs", &self.exprs().collect::<Vec<_>>())
            .finish()
    }
}

pub struct UnaryExpr(SyntaxNode);

impl UnaryExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::PrefixExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn expr(&self) -> Option<Expression> {
        self.0.children().find_map(Expression::cast)
    }

    #[must_use]
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Minus)
    }
}

impl fmt::Debug for UnaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnaryExpr")
            .field("op", &self.op())
            .field("expr", &self.expr())
            .finish()
    }
}

pub struct LambdaExpr(SyntaxNode);

impl LambdaExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::LambdaExprDef {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn args(&self) -> Vec<Pattern> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::LambdaExprArgList))
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
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::LambdaExprBody))
            .and_then(|parent| parent.children().find_map(Expression::cast))
    }
}

impl fmt::Debug for LambdaExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LambdaExpr")
            .field("args", &self.args())
            .field("body", &self.body())
            .finish()
    }
}

pub struct LambdaExprArg(SyntaxNode);

impl LambdaExprArg {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::LambdaExprArg {
            Some(Self(node))
        } else {
            None
        }
    }

    fn pattern(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
}

impl fmt::Debug for LambdaExprArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LambdaExprArg")
            .field("pattern", &self.pattern())
            .finish()
    }
}

pub struct FunctionCall(SyntaxNode);

impl FunctionCall {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FunctionCall {
            Some(Self(node))
        } else {
            None
        }
    }

    fn target(&self) -> Option<FunctionCallTarget> {
        self.0.children().find_map(FunctionCallTarget::cast)
    }

    fn args(&self) -> impl Iterator<Item = FunctionCallArg> {
        self.0
            .children()
            .filter(|node| node.kind() == SyntaxKind::FunctionCallArgList)
            .flat_map(|parent| parent.children().filter_map(FunctionCallArg::cast))
    }
}

impl fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionCall")
            .field("target", &self.target())
            .field("args", &self.args().collect::<Vec<_>>())
            .finish()
    }
}

pub struct FunctionCallTarget(SyntaxNode);

impl FunctionCallTarget {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FunctionCallTarget {
            Some(Self(node))
        } else {
            None
        }
    }

    fn target(&self) -> Option<VariableRef> {
        self.0.children().find_map(VariableRef::cast)
    }
}

impl fmt::Debug for FunctionCallTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionCallTarget")
            .field("target", &self.target())
            .finish()
    }
}

pub struct FunctionCallArg(SyntaxNode);

impl FunctionCallArg {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FunctionCallArg {
            Some(Self(node))
        } else {
            None
        }
    }

    fn expression(&self) -> Option<Expression> {
        self.0.children().find_map(Expression::cast)
    }
}

impl fmt::Debug for FunctionCallArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionCallArg")
            .field("expression", &self.expression())
            .finish()
    }
}

pub struct MatchExpr(SyntaxNode);

impl MatchExpr {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::MatchExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn expression(&self) -> Option<Expression> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::MatchExprArg {
                token.children().find_map(Expression::cast)
            } else {
                None
            }
        })
    }

    pub fn targets(&self) -> impl Iterator<Item = MatchTarget> {
        self.0.children().filter_map(MatchTarget::cast)
    }
}

impl fmt::Debug for MatchExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MatchExpr")
            .field("expression", &self.expression())
            .field("targets", &self.targets().collect::<Vec<_>>())
            .finish()
    }
}

pub struct MatchTarget(SyntaxNode);

impl MatchTarget {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::MatchTarget {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn condition(&self) -> Option<Pattern> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::MatchTargetCondition {
                token.children().find_map(Pattern::cast)
            } else {
                None
            }
        })
    }

    pub fn value(&self) -> Option<Expression> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::MatchTargetValue {
                token.children().find_map(Expression::cast)
            } else {
                None
            }
        })
    }
}

impl fmt::Debug for MatchTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MatchTarget")
            .field("condition", &self.condition())
            .field("value", &self.value())
            .finish()
    }
}
