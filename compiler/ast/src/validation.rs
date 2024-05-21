use std::fmt;

use text_size::TextRange;

use alloy_syntax::SyntaxNode;

use crate::ast::{AstElement, CharLiteral, IntLiteral};

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, PartialEq)]
pub struct ValidationError {
    kind: ValidationErrorKind,
    range: TextRange,
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.kind,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValidationErrorKind {
    NumberLiteralTooLarge,
    CharLiteralInvalid,
}

impl fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NumberLiteralTooLarge => write!(
                f,
                "number literal is larger than an integer’s maximum value, {}",
                u64::MAX,
            ),
            Self::CharLiteralInvalid => todo!(),
        }
    }
}

#[must_use]
pub fn validate(node: &SyntaxNode) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    for node in node.descendants() {
        if let Some(literal) = IntLiteral::cast(node.clone()) {
            validate_int_literal(&literal, &mut errors);
        }
        if let Some(literal) = CharLiteral::cast(node) {
            validate_char_literal(&literal, &mut errors);
        }
    }

    errors
}

fn validate_int_literal(literal: &IntLiteral, errors: &mut Vec<ValidationError>) {
    if literal.value().is_none() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::NumberLiteralTooLarge,
            range: literal.range(),
        });
    }
}

fn validate_char_literal(literal: &CharLiteral, errors: &mut Vec<ValidationError>) {
    if literal.value().is_none() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::CharLiteralInvalid,
            range: literal.range(),
        });
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range as StdRange;

    use super::*;

    #[track_caller]
    fn check(input: &str, expected_errors: &[(ValidationErrorKind, StdRange<u32>)]) {
        let parse = alloy_parser::parse_source_file(input);

        let expected_errors: Vec<_> = expected_errors
            .iter()
            .map(|(kind, range)| ValidationError {
                kind: *kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        assert_eq!(validate(&parse.syntax()), expected_errors);
    }

    #[test]
    fn validate_ok_literal() {
        check("123", &[]);
    }

    #[test]
    fn validate_too_large_literal() {
        check(
            "99999999999999999999",
            &[(ValidationErrorKind::NumberLiteralTooLarge, (0..20))],
        );
    }

    #[test]
    fn lower_multi_char_literal() {
        check(
            "'characters'",
            &[(ValidationErrorKind::CharLiteralInvalid, (0..12))],
        );
        check("''", &[(ValidationErrorKind::CharLiteralInvalid, (0..2))]);
    }
}
