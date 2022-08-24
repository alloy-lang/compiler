use alloy_rowan_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use ordered_float::NotNan;

pub mod validation;

#[derive(Debug)]
pub struct VariableDef(SyntaxNode);

impl VariableDef {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct BinaryExpr(SyntaxNode);

impl BinaryExpr {
    #[must_use]
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    #[must_use]
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| {
                matches!(
                    token.kind(),
                    SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash,
                )
            })
    }
}

#[derive(Debug)]
pub struct IntLiteral(SyntaxNode);

impl IntLiteral {
    #[must_use]
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IntLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    #[must_use]
    pub fn parse(&self) -> Option<u64> {
        self.0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .parse()
            .ok()
    }
}

#[derive(Debug)]
pub struct FractionalLiteral(SyntaxNode);

impl FractionalLiteral {
    #[must_use]
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FractionalLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    #[must_use]
    pub fn parse(&self) -> Option<NotNan<f64>> {
        self.0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .parse()
            .ok()
    }
}

#[derive(Debug)]
pub struct StringLiteral(SyntaxNode);

impl StringLiteral {
    #[must_use]
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::StringLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    #[must_use]
    pub fn parse(&self) -> String {
        let string = self
            .0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .to_string();

        string[1..string.len() - 1].to_string()
    }
}

#[derive(Debug)]
pub struct CharLiteral(SyntaxNode);

impl CharLiteral {
    #[must_use]
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::CharLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    #[must_use]
    pub fn parse(&self) -> Option<char> {
        self.0
            .first_token()
            .iter()
            .filter(|token| token.text().len() == 3)
            .find_map(|token| token.text().chars().nth(1))
    }
}

#[derive(Debug)]
pub struct IfThenElseExpr(SyntaxNode);

impl IfThenElseExpr {
    #[must_use]
    pub fn cond(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::IfExpr))
            .and_then(|parent| parent.children().find_map(Expr::cast))
    }

    #[must_use]
    pub fn then(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::ThenExpr))
            .and_then(|parent| parent.children().find_map(Expr::cast))
    }

    #[must_use]
    pub fn else_(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::ElseExpr))
            .and_then(|parent| parent.children().find_map(Expr::cast))
    }
}

#[derive(Debug)]
pub struct UnitExpr(SyntaxNode);

impl UnitExpr {}

#[derive(Debug)]
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct TupleExpr(SyntaxNode);

impl TupleExpr {
    #[must_use]
    pub fn exprs(&self) -> Vec<Expr> {
        self.0.children().filter_map(Expr::cast).collect()
    }
}

#[derive(Debug)]
pub struct UnaryExpr(SyntaxNode);

impl UnaryExpr {
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    #[must_use]
    pub fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Minus)
    }
}

#[derive(Debug)]
pub struct VariableRef(SyntaxNode);

impl VariableRef {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }
}

#[derive(Debug)]
pub struct LambdaExpr(SyntaxNode);

impl LambdaExpr {
    #[must_use]
    pub fn args(&self) -> Vec<Pattern> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::LambdaArgList))
            .map(|parent| {
                parent
                    .children()
                    .filter_map(|c| LambdaArg::cast(&c))
                    .filter_map(|c| c.pattern())
                    .collect()
            })
            .unwrap_or_default()
    }

    #[must_use]
    pub fn body(&self) -> Option<Expr> {
        self.0
            .children()
            .find(|node| matches!(node.kind(), SyntaxKind::LambdaExprBody))
            .and_then(|parent| parent.children().find_map(Expr::cast))
    }
}

#[derive(Debug)]
pub struct LambdaArg(SyntaxNode);

impl LambdaArg {
    #[must_use]
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::LambdaArg {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    fn pattern(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
}

#[derive(Debug)]
pub enum Pattern {
    IntLiteral(IntLiteral),
    FractionalLiteral(FractionalLiteral),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    VariableRef(VariableRef),
}

impl Pattern {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::FractionalLiteral => Self::FractionalLiteral(FractionalLiteral(node)),
            SyntaxKind::StringLiteral => Self::StringLiteral(StringLiteral(node)),
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral(node)),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef(node)),
            _ => return None,
        };

        Some(result)
    }
}

#[derive(Debug)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    IntLiteral(IntLiteral),
    FractionalLiteral(FractionalLiteral),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    IfThenElseExpr(IfThenElseExpr),
    UnitExpr(UnitExpr),
    ParenExpr(ParenExpr),
    TupleExpr(TupleExpr),
    UnaryExpr(UnaryExpr),
    VariableRef(VariableRef),
    LambdaExpr(LambdaExpr),
}

impl Expr {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::InfixExpr => Self::BinaryExpr(BinaryExpr(node)),
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::FractionalLiteral => Self::FractionalLiteral(FractionalLiteral(node)),
            SyntaxKind::StringLiteral => Self::StringLiteral(StringLiteral(node)),
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral(node)),
            SyntaxKind::IfThenElseExpr => Self::IfThenElseExpr(IfThenElseExpr(node)),
            SyntaxKind::UnitExpr => Self::UnitExpr(UnitExpr(node)),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr(node)),
            SyntaxKind::TupleExpr => Self::TupleExpr(TupleExpr(node)),
            SyntaxKind::PrefixExpr => Self::UnaryExpr(UnaryExpr(node)),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef(node)),
            SyntaxKind::LambdaExprDef => Self::LambdaExpr(LambdaExpr(node)),
            _ => return None,
        };

        Some(result)
    }
}

#[derive(Debug)]
pub enum Stmt {
    VariableDef(VariableDef),
    Expr(Expr),
}

impl Stmt {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef(node)),
            _ => Self::Expr(Expr::cast(node)?),
        };

        Some(result)
    }
}

#[derive(Debug)]
pub struct SourceFile(SyntaxNode);

impl SourceFile {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::SourceFile {
            Some(Self(node))
        } else {
            None
        }
    }
}

impl SourceFile {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}
