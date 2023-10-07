use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use alloy_lexer::TokenKind;

pub type SyntaxNode = rowan::SyntaxNode<AlloyLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<AlloyLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<AlloyLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<AlloyLanguage>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    Whitespace,
    LetKw,
    ImportKw,
    ModuleKw,
    WhereKw,
    ForKw,
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
    Fraction,
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
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    NilIdentifier,
    Pipe,
    Comment,
    Error,

    SourceFile,
    ModuleDef,
    InfixExpr,
    IntLiteral,
    FractionLiteral,
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
    UnaryExpr,
    ValueDef,
    VariableRef,
    LambdaExpr,
    LambdaExprArgList,
    LambdaExprArg,
    LambdaExprBody,
    ImportDef,
    ImportDefSegment,
    ImportDefGroup,
    FunctionCall,
    FunctionCallArgList,
    FunctionCallArg,
    Destructor,
    DestructorTarget,
    DestructorArg,
    Path,
    TraitDef,
    BehaviorDef,
    BehaviorTraitName,
    BehaviorTypeName,
    TypeAnnotation,
    TypeDefinition,
    TypeDefinitionMember,
    TypeDefinitionMemberProperty,
    LambdaType,
    SelfType,
    UnitType,
    ParenthesizedType,
    TupleType,
    TypeIdentifier,
    VariableType,
    NamedTypeVariable,
    SelfTypeVariable,
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
            TokenKind::ForKw => Self::ForKw,
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
            TokenKind::Fraction => Self::Fraction,
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
            TokenKind::LBracket => Self::LBracket,
            TokenKind::RBracket => Self::RBracket,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::RAngle => Self::RAngle,
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
