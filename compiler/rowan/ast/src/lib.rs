use alloy_rowan_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use ordered_float::NotNan;

pub mod validation;

#[derive(Debug)]
pub struct VariableDef(SyntaxNode);

impl VariableDef {
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
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

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
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IntLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    pub fn parse(&self) -> Option<u64> {
        self.0.first_token().unwrap().text().parse().ok()
    }
}

#[derive(Debug)]
pub struct FractionalLiteral(SyntaxNode);

impl FractionalLiteral {
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FractionalLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    pub fn parse(&self) -> Option<NotNan<f64>> {
        self.0.first_token().unwrap().text().parse().ok()
    }
}

#[derive(Debug)]
pub struct StringLiteral(SyntaxNode);

impl StringLiteral {
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::StringLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    pub fn parse(&self) -> String {
        let string = self.0.first_token().unwrap().text().to_string();

        string[1..string.len() - 1].to_string()
    }
}

#[derive(Debug)]
pub struct CharLiteral(SyntaxNode);

impl CharLiteral {
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::CharLiteral {
            Some(Self(node.clone()))
        } else {
            None
        }
    }

    pub fn parse(&self) -> Option<char> {
        self.0
            .first_token()
            .iter()
            .filter(|token| token.text().len() == 3)
            .filter_map(|token| token.text().chars().nth(1))
            .next()
    }
}

#[derive(Debug)]
pub struct ParenExpr(SyntaxNode);

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct UnaryExpr(SyntaxNode);

impl UnaryExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

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
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.first_token()
    }
}

#[derive(Debug)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    IntLiteral(IntLiteral),
    FractionalLiteral(FractionalLiteral),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    ParenExpr(ParenExpr),
    UnaryExpr(UnaryExpr),
    VariableRef(VariableRef),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::InfixExpr => Self::BinaryExpr(BinaryExpr(node)),
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral(node)),
            SyntaxKind::FractionalLiteral => Self::FractionalLiteral(FractionalLiteral(node)),
            SyntaxKind::StringLiteral => Self::StringLiteral(StringLiteral(node)),
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral(node)),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr(node)),
            SyntaxKind::PrefixExpr => Self::UnaryExpr(UnaryExpr(node)),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef(node)),
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
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef(node)),
            _ => Self::Expr(Expr::cast(node)?),
        };

        Some(result)
    }
}

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}
