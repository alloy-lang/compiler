use alloy_rowan_lexer::TokenKind;
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
    LambdaArgComma,
    LambdaArgExpr,
    LambdaArgPipe,
    LambdaExprRightArrow,
    LambdaExprExpr,
    PrefixExprExpr,
    ParenExprExpr,
    ParenExprComma,
    ParenExprRightParen,
    IfThenElseIfExpr,
    IfThenElseThenKw,
    IfThenElseThenExpr,
    IfThenElseElseKw,
    IfThenElseElseExpr,
    VariableDefIdent,
    VariableDefEquals,
    VariableDefExpr,
    ImportStatementFirstSegment,
    ImportStatementSegment,
    ImportStatementSeparator,
    ImportStatementGroupSeparator,
    ImportStatementGroupEnd,
    TraitName,
    TraitWhere,
    TraitEnd,
    TypeOfName,
    TypeOfColon,
    SingleType,
    TypeVariableName,
    TopLevelExpr,
}

impl ParseErrorContext {
    #[must_use]
    fn long_message<'a>(&self) -> &'a str {
        match self {
            ParseErrorContext::LambdaArgComma => {
                "We expected to see a comma separating the arguments."
            }
            ParseErrorContext::LambdaArgExpr => "We expected to see an expression.",
            ParseErrorContext::LambdaArgPipe => {
                "We expected to see a pipe at the end of the arguments."
            }
            ParseErrorContext::LambdaExprRightArrow => {
                "We expected to see a right arrow after the lambda arguments."
            }
            ParseErrorContext::LambdaExprExpr => {
                "We expected to see an expression after the lambda arguments."
            }
            ParseErrorContext::PrefixExprExpr => {
                "We expected to see an expression after the prefix operator."
            }
            ParseErrorContext::ParenExprExpr => "We expected to see an expression after the ‘(‘.",
            ParseErrorContext::ParenExprComma => todo!(),
            ParseErrorContext::ParenExprRightParen => {
                "We expected to see a right parenthesis after the expression."
            }
            ParseErrorContext::IfThenElseIfExpr => {
                "We expected to see an expression after the ‘if‘."
            }
            ParseErrorContext::IfThenElseThenKw => {
                "We expected to see a ‘then‘ after the ‘if‘ expression."
            }
            ParseErrorContext::IfThenElseThenExpr => {
                "We expected to see an expression after the ‘then‘."
            }
            ParseErrorContext::IfThenElseElseKw => {
                "We expected to see a ‘else‘ after the ‘then‘ expression."
            }
            ParseErrorContext::IfThenElseElseExpr => {
                "We expected to see an expression after the ‘else‘."
            }
            ParseErrorContext::VariableDefIdent => {
                "We expected to see an identifier after the ‘let‘."
            }
            ParseErrorContext::VariableDefEquals => {
                "We expected to see an ‘=‘ after the identifier."
            }
            ParseErrorContext::VariableDefExpr => "We expected to see an expression after the ‘=‘.",
            ParseErrorContext::ImportStatementFirstSegment => todo!(),
            ParseErrorContext::ImportStatementSegment => todo!(),
            ParseErrorContext::ImportStatementSeparator => todo!(),
            ParseErrorContext::ImportStatementGroupSeparator => todo!(),
            ParseErrorContext::ImportStatementGroupEnd => todo!(),
            ParseErrorContext::TraitName => todo!(),
            ParseErrorContext::TraitWhere => todo!(),
            ParseErrorContext::TraitEnd => todo!(),
            ParseErrorContext::TypeOfName => todo!(),
            ParseErrorContext::TypeOfColon => todo!(),
            ParseErrorContext::SingleType => todo!(),
            ParseErrorContext::TopLevelExpr => "We expected to see an expression.",
        }
    }

    #[must_use]
    fn context_name<'a>(&self) -> &'a str {
        match self {
            ParseErrorContext::LambdaArgComma => "a lambda argument",
            ParseErrorContext::LambdaArgExpr => "a lambda argument",
            ParseErrorContext::LambdaArgPipe => "a lambda argument",
            ParseErrorContext::LambdaExprRightArrow => "a lambda expression",
            ParseErrorContext::LambdaExprExpr => "a lambda expression body",
            ParseErrorContext::PrefixExprExpr => "an expression after a prefix operator",
            ParseErrorContext::ParenExprExpr => "an expression inside parentheses",
            ParseErrorContext::ParenExprComma => "a comma between expressions inside parentheses",
            ParseErrorContext::ParenExprRightParen => "a close parenthesis after an expression",
            ParseErrorContext::IfThenElseIfExpr => "an if-then-else expression",
            ParseErrorContext::IfThenElseThenKw => "an if-then-else expression",
            ParseErrorContext::IfThenElseThenExpr => "an if-then-else expression",
            ParseErrorContext::IfThenElseElseKw => "an if-then-else expression",
            ParseErrorContext::IfThenElseElseExpr => "an if-then-else expression",
            ParseErrorContext::VariableDefIdent => "a variable definition",
            ParseErrorContext::VariableDefEquals => "a variable definition",
            ParseErrorContext::VariableDefExpr => "a variable definition",
            ParseErrorContext::ImportStatementFirstSegment => "the first import segment",
            ParseErrorContext::ImportStatementSegment => "an import statement",
            ParseErrorContext::ImportStatementSeparator => todo!(),
            ParseErrorContext::ImportStatementGroupSeparator => {
                "the comma between imports inside an import group"
            }
            ParseErrorContext::ImportStatementGroupEnd => "the end of an import group",
            ParseErrorContext::TraitName => "the name of the trait in a trait definition",
            ParseErrorContext::TraitWhere => "the where keyword at the start of a trait definition",
            ParseErrorContext::TraitEnd => "the end keyword after a trait definition",
            ParseErrorContext::TypeOfName => todo!(),
            ParseErrorContext::TypeOfColon => todo!(),
            ParseErrorContext::SingleType => "the type of a type annotation",
            ParseErrorContext::TopLevelExpr => "a top level expression",
        }
    }

    #[must_use]
    fn short_message<'a>(&self) -> &'a str {
        match self {
            ParseErrorContext::LambdaArgComma => "expected ‘,‘ separating arguments",
            ParseErrorContext::LambdaArgExpr => "expected expression as argument",
            ParseErrorContext::LambdaArgPipe => "expected a ‘|‘ after the last argument",
            ParseErrorContext::LambdaExprRightArrow => "expected ‘->‘ after the arguments",
            ParseErrorContext::LambdaExprExpr => "expected expression as body",
            ParseErrorContext::PrefixExprExpr => todo!(),
            ParseErrorContext::ParenExprExpr => todo!(),
            ParseErrorContext::ParenExprComma => todo!(),
            ParseErrorContext::ParenExprRightParen => "expected ‘)‘ after expression",
            ParseErrorContext::IfThenElseIfExpr => todo!(),
            ParseErrorContext::IfThenElseThenKw => "expected ‘then‘ after ‘if‘ expression",
            ParseErrorContext::IfThenElseThenExpr => todo!(),
            ParseErrorContext::IfThenElseElseKw => "expected ‘else‘ after ‘then‘ expression",
            ParseErrorContext::IfThenElseElseExpr => todo!(),
            ParseErrorContext::VariableDefIdent => todo!(),
            ParseErrorContext::VariableDefEquals => todo!(),
            ParseErrorContext::VariableDefExpr => todo!(),
            ParseErrorContext::ImportStatementFirstSegment => todo!(),
            ParseErrorContext::ImportStatementSegment => todo!(),
            ParseErrorContext::ImportStatementSeparator => todo!(),
            ParseErrorContext::ImportStatementGroupSeparator => todo!(),
            ParseErrorContext::ImportStatementGroupEnd => todo!(),
            ParseErrorContext::TraitName => todo!(),
            ParseErrorContext::TraitWhere => todo!(),
            ParseErrorContext::TraitEnd => todo!(),
            ParseErrorContext::TypeOfName => todo!(),
            ParseErrorContext::TypeOfColon => todo!(),
            ParseErrorContext::SingleType => todo!(),
            ParseErrorContext::TopLevelExpr => todo!(),
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
