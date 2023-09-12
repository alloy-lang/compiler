use alloy_lexer::TokenKind;
use std::fmt;
use text_size::{TextRange, TextSize};

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) kind: ParseErrorKind,
    pub(super) context: ParseErrorContext,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseErrorKind {
    Missing { offset: TextSize },
    Unexpected { found: TokenKind, range: TextRange },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ParseErrorContext {
    ModuleName,
    ModuleWhere,
    LambdaArgComma,
    LambdaArgExpr,
    LambdaArgPipe,
    LambdaExprRightArrow,
    LambdaExprExpr,
    FunctionCallArgExpr,
    FunctionCallArgComma,
    FunctionCallRightParen,
    PrefixExprExpr,
    ParenExprExpr,
    ParenExprComma,
    ParenExprRightParen,
    IfThenElseIfExpr,
    IfThenElseThenKw,
    IfThenElseThenExpr,
    IfThenElseElseKw,
    IfThenElseElseExpr,
    MatchExprArg,
    MatchExprWhenKw,
    MatchTargetPipe,
    MatchTargetRightArrow,
    MatchTargetCondition,
    MatchTargetValue,
    VariableDefIdent,
    VariableDefEquals,
    VariableDefExpr,
    VariableRef,
    ImportStatementFirstSegment,
    ImportStatementSegment,
    ImportStatementSeparator,
    ImportStatementGroupSeparator,
    ImportStatementGroupEnd,
    TraitName,
    TraitWhere,
    TraitEnd,
    TraitMemberFirst,
    TypeOfName,
    TypeOfColon,
    TypeDefName,
    TypeDefEquals,
    TypeDefMemberName,
    TypeDefMemberPipe,
    TypeDefGenericConstraintName,
    UnitTypeRightParen,
    ParenthesizedTypeRightParen,
    TupleTypeRightParen,
    SingleType,
    SelfTypeOutsideContext,
    TypeVariableName,
    TraitSelfConstraintsEquals,
    TypeVariableConstraint,
    TypeVariableConstraintPlus,
    TypeVariableTraitConstraint,
    TypeVariableKindConstraintTypeKw,
    TypeVariableKindConstraintLAngle,
    TypeVariableKindConstraintUnderscore,
    TypeVariableKindConstraintUnderscoreComma,
    TypeVariableKindConstraintRAngle,
    BoundedTypeLAngle,
    BoundedTypeComma,
    BoundedTypeName,
    BoundedTypeRAngle,
    TopLevelExpr,
}

impl ParseErrorContext {
    #[must_use]
    fn context_name<'a>(self) -> &'a str {
        match self {
            ParseErrorContext::ModuleName => "the name of the module in a module definition",
            ParseErrorContext::ModuleWhere => {
                "the where keyword at the start of a module definition"
            }
            ParseErrorContext::LambdaArgComma => "a comma between arguments in a lambda expression",
            ParseErrorContext::LambdaArgExpr => "an argument in a lambda expression",
            ParseErrorContext::LambdaArgPipe => {
                "the pipe at the end of a lambda expression's argument list"
            }
            ParseErrorContext::LambdaExprRightArrow => "a lambda expression",
            ParseErrorContext::LambdaExprExpr => "a lambda expression body",
            ParseErrorContext::FunctionCallArgExpr => "a function call argument",
            ParseErrorContext::FunctionCallArgComma => {
                "a comma between arguments in a function call"
            }
            ParseErrorContext::FunctionCallRightParen => {
                "a close parenthesis after a function call"
            }
            ParseErrorContext::PrefixExprExpr => "an expression after a prefix operator",
            ParseErrorContext::ParenExprExpr => "an expression inside parentheses",
            ParseErrorContext::ParenExprComma => "a comma between expressions inside parentheses",
            ParseErrorContext::ParenExprRightParen => "a close parenthesis after an expression",
            ParseErrorContext::IfThenElseIfExpr => {
                "the conditional expression in an if-then-else expression"
            }
            ParseErrorContext::IfThenElseThenKw => {
                "the `then` keyword in an if-then-else expression"
            }
            ParseErrorContext::IfThenElseThenExpr => {
                "the `then` expression in an if-then-else expression"
            }
            ParseErrorContext::IfThenElseElseKw => {
                "the `else` keyword in an if-then-else expression"
            }
            ParseErrorContext::IfThenElseElseExpr => {
                "the `else` expression in an if-then-else expression"
            }
            ParseErrorContext::MatchExprArg => "the argument of a match expression",
            ParseErrorContext::MatchExprWhenKw => "the when keyword of a match expression",
            ParseErrorContext::MatchTargetPipe => {
                "the `|` at the start of a match expression branch"
            }
            ParseErrorContext::MatchTargetRightArrow => "the `->` in a match expression branch",
            ParseErrorContext::MatchTargetCondition => "the condition in a match expression branch",
            ParseErrorContext::MatchTargetValue => "the value in a match expression branch",
            ParseErrorContext::VariableDefIdent => "a variable definition",
            ParseErrorContext::VariableDefEquals => "a variable definition",
            ParseErrorContext::VariableDefExpr => "a variable definition",
            ParseErrorContext::VariableRef => "a variable reference",
            ParseErrorContext::ImportStatementFirstSegment => "the first import segment",
            ParseErrorContext::ImportStatementSegment => "an import statement",
            ParseErrorContext::ImportStatementSeparator => "the ‘::’ between import segments",
            ParseErrorContext::ImportStatementGroupSeparator => {
                "the comma between imports inside an import group"
            }
            ParseErrorContext::ImportStatementGroupEnd => "the end of an import group",
            ParseErrorContext::TraitName => "the name of the trait in a trait definition",
            ParseErrorContext::TraitWhere => "the where keyword at the start of a trait definition",
            ParseErrorContext::TraitEnd => "the end keyword after a trait definition",
            ParseErrorContext::TraitMemberFirst => {
                "the start of a trait member, are you missing a keyword?"
            }
            ParseErrorContext::TypeOfName => "the name of a type",
            ParseErrorContext::TypeOfColon => "the colon after the name of a type",
            ParseErrorContext::TypeDefName => "the name in a type definition",
            ParseErrorContext::TypeDefEquals => "the equals sign in a type definition",
            ParseErrorContext::TypeDefMemberName => "the name of a type definition member",
            ParseErrorContext::TypeDefMemberPipe => todo!(),
            ParseErrorContext::TypeDefGenericConstraintName => {
                "a generic constraint in a type definition"
            }
            ParseErrorContext::UnitTypeRightParen => "the right parenthesis of a unit type",
            ParseErrorContext::ParenthesizedTypeRightParen => {
                "the right parenthesis of a parenthesized type"
            }
            ParseErrorContext::TupleTypeRightParen => "the right parenthesis of a tuple type",
            ParseErrorContext::SingleType => "the type of a type annotation",
            ParseErrorContext::SelfTypeOutsideContext => "a type, encountered an unexpected `self` reference outside a trait or behavior context",
            ParseErrorContext::TypeVariableName => "the name of a type variable",
            ParseErrorContext::TraitSelfConstraintsEquals => {
                "the `=` sign in a trait `self` constraint"
            }
            ParseErrorContext::TypeVariableConstraint => "the constraints for a type variable",
            ParseErrorContext::TypeVariableConstraintPlus => "the `+` between type constraints",
            ParseErrorContext::TypeVariableTraitConstraint => {
                "the trait name in a type variable constraint"
            }
            ParseErrorContext::TypeVariableKindConstraintTypeKw => {
                "the `Type` keyword before the <_> pattern thingy"
            }
            ParseErrorContext::TypeVariableKindConstraintLAngle => {
                "the `<` at the start of the <_> pattern thingy"
            }
            ParseErrorContext::TypeVariableKindConstraintUnderscore => {
                "the `_` in the <_> pattern thingy"
            }
            ParseErrorContext::TypeVariableKindConstraintUnderscoreComma => {
                "the comma between `_` in the <_> pattern thingy"
            }
            ParseErrorContext::TypeVariableKindConstraintRAngle => {
                "the > at the end of the <_> pattern thingy"
            }
            ParseErrorContext::BoundedTypeLAngle => {
                "the `<` at the start of the bounded type arguments"
            }
            ParseErrorContext::BoundedTypeComma => "the comma between arguments in a bounded type",
            ParseErrorContext::BoundedTypeRAngle => {
                "the `>` at the end of the bounded type arguments"
            }
            ParseErrorContext::BoundedTypeName => "the name of a bounded type argument",
            ParseErrorContext::TopLevelExpr => "a top level expression",
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let context_name = self.context.context_name();

        match self.kind {
            ParseErrorKind::Missing { offset } => {
                write!(f, "error at position {:?}", offset)?;
                write!(f, " while parsing {}. ", context_name)?;
                f.write_str("Missing expected ")?;
            }
            ParseErrorKind::Unexpected { found, range } => {
                write!(
                    f,
                    "error in range {}..{}",
                    u32::from(range.start()),
                    u32::from(range.end()),
                )?;
                write!(f, " while parsing {}. ", context_name)?;
                write!(f, "Found {}, but expected ", found)?;
            }
        }

        //
        // Expected
        //

        let num_expected = self.expected.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if is_first(idx) {
                write!(f, "{}", expected_kind)?;
            } else if is_last(idx) {
                write!(f, " or {}", expected_kind)?;
            } else {
                write!(f, ", {}", expected_kind)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn check(expected: Vec<TokenKind>, kind: ParseErrorKind, output: &str) {
        let error = ParseError {
            expected,
            kind,
            context: ParseErrorContext::ParenExprExpr,
        };

        assert_eq!(format!("{}", error), output);
    }

    #[test]
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Equals],
            ParseErrorKind::Missing {
                offset: TextSize::from(20),
            },
            "error at position 20 while parsing an expression inside parentheses. Missing expected ‘=’",
        );
    }

    #[test]
    fn one_expected_did_not_find() {
        check(
            vec![TokenKind::RParen],
            ParseErrorKind::Missing {
                offset: TextSize::from(6),
            },
            "error at position 6 while parsing an expression inside parentheses. Missing expected ‘)’",
        );
    }

    #[test]
    fn multiple_expected_did_find() {
        check(
            vec![
                TokenKind::Integer,
                TokenKind::Ident,
                TokenKind::Minus,
                TokenKind::LParen,
            ],
            ParseErrorKind::Unexpected {
                found: TokenKind::LetKw,
                range: TextRange::new(100.into(), 105.into()),
            },
            "error in range 100..105 while parsing an expression inside parentheses. Found ‘let’, but expected integer, identifier, ‘-’ or ‘(’",
        );
    }

    #[test]
    fn multiple_expected_did_not_find() {
        check(
            vec![TokenKind::Plus, TokenKind::Minus],
            ParseErrorKind::Missing {
                offset: TextSize::from(1),
            },
            "error at position 1 while parsing an expression inside parentheses. Missing expected ‘+’ or ‘-’",
        );
    }
}
