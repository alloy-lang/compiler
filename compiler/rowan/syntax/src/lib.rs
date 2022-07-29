use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use alloy_rowan_lexer::TokenKind;

pub type SyntaxNode = rowan::SyntaxNode<AlloyLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<AlloyLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<AlloyLanguage>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    Whitespace,
    LetKw,
    IfKw,
    ThenKw,
    ElseKw,
    Ident,
    Integer,
    Fractional,
    String,
    Char,
    RightArrow,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Pipe,
    Comment,
    Error,

    Root,
    InfixExpr,
    IntLiteral,
    FractionalLiteral,
    StringLiteral,
    CharLiteral,
    IfThenElseExpr,
    IfExpr,
    ThenExpr,
    ElseExpr,
    ParenExpr,
    PrefixExpr,
    VariableDef,
    VariableRef,
    LambdaExprDef,
    LambdaArgList,
    LambdaArg,
    LambdaExprBody,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::LetKw => Self::LetKw,
            TokenKind::IfKw => Self::IfKw,
            TokenKind::ThenKw => Self::ThenKw,
            TokenKind::ElseKw => Self::ElseKw,
            TokenKind::Ident => Self::Ident,
            TokenKind::Integer => Self::Integer,
            TokenKind::Fractional => Self::Fractional,
            TokenKind::String => Self::String,
            TokenKind::Char => Self::Char,
            TokenKind::RightArrow => Self::RightArrow,
            TokenKind::Comma => Self::Comma,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Equals => Self::Equals,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::Pipe => Self::Pipe,
            TokenKind::Comment => Self::Comment,
            TokenKind::Error => Self::Error,
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum AlloyLanguage {}

impl rowan::Language for AlloyLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}
