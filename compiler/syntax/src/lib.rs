use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use alloy_lexer::TokenKind;

pub type SyntaxNode = rowan::SyntaxNode<AlloyLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<AlloyLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<AlloyLanguage>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    Whitespace,
    LetKw,
    ImportKw,
    ModuleKw,
    WhereKw,
    WhenKw,
    MatchKw,
    TraitKw,
    BehaviorKw,
    TypedefKw,
    TypevarKw,
    TypeOfKw,
    IfKw,
    ThenKw,
    ElseKw,
    SelfKw,
    EndKw,
    TypeKw,
    Ident,
    OpIdent,
    Integer,
    Fractional,
    String,
    Char,
    Colon,
    DoubleColon,
    Comma,
    RightArrow,
    Hash,
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    ClosedAngle,
    NilIdentifier,
    Pipe,
    Comment,
    Error,

    SourceFile,
    ModuleDef,
    InfixExpr,
    IntLiteral,
    FractionalLiteral,
    StringLiteral,
    CharLiteral,
    IfThenElseExpr,
    IfExpr,
    ThenExpr,
    ElseExpr,
    MatchExpr,
    MatchExprArg,
    MatchTarget,
    MatchTargetCondition,
    MatchTargetValue,
    UnitExpr,
    ParenExpr,
    TupleExpr,
    TupleExprArg,
    PrefixExpr,
    VariableDef,
    VariableRef,
    LambdaExprDef,
    LambdaArgList,
    LambdaArg,
    LambdaExprBody,
    ImportStatement,
    ImportStatementSegment,
    ImportStatementGroup,
    FunctionCall,
    FunctionCallTarget,
    FunctionCallArgList,
    FunctionCallArg,
    TraitDef,
    TypeAnnotation,
    LambdaType,
    SelfType,
    UnitType,
    ParenthesizedType,
    TupleType,
    TypeIdentifier,
    VariableType,
    TypeVariable,
    TypeVariableKindConstraint,
    TypeVariableTraitConstraint,
    BoundedType,
    BoundedTypeBase,
    BoundedTypeArg,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::LetKw => Self::LetKw,
            TokenKind::ImportKw => Self::ImportKw,
            TokenKind::ModuleKw => Self::ModuleKw,
            TokenKind::WhereKw => Self::WhereKw,
            TokenKind::WhenKw => Self::WhenKw,
            TokenKind::MatchKw => Self::MatchKw,
            TokenKind::TraitKw => Self::TraitKw,
            TokenKind::BehaviorKw => Self::BehaviorKw,
            TokenKind::TypedefKw => Self::TypedefKw,
            TokenKind::TypevarKw => Self::TypevarKw,
            TokenKind::TypeOfKw => Self::TypeOfKw,
            TokenKind::IfKw => Self::IfKw,
            TokenKind::ThenKw => Self::ThenKw,
            TokenKind::ElseKw => Self::ElseKw,
            TokenKind::SelfKw => Self::SelfKw,
            TokenKind::EndKw => Self::EndKw,
            TokenKind::TypeKw => Self::TypeKw,
            TokenKind::Ident => Self::Ident,
            TokenKind::OpIdent => Self::OpIdent,
            TokenKind::Integer => Self::Integer,
            TokenKind::Fractional => Self::Fractional,
            TokenKind::String => Self::String,
            TokenKind::Char => Self::Char,
            TokenKind::Colon => Self::Colon,
            TokenKind::DoubleColon => Self::DoubleColon,
            TokenKind::RightArrow => Self::RightArrow,
            TokenKind::Comma => Self::Comma,
            TokenKind::Hash => Self::Hash,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Equals => Self::Equals,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::RAngle => Self::RAngle,
            TokenKind::ClosedAngle => Self::ClosedAngle,
            TokenKind::NilIdentifier => Self::NilIdentifier,
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