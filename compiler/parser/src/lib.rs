use core::convert;
use std::iter;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use improved_slice_patterns::match_vec;
use non_empty_vec::NonEmpty;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, TokenStream, T};

mod docs;
mod expr;
mod parens;
mod pattern;
mod r#type;

//
// parse functions
//

pub fn parse(source: &str) -> Result<Spanned<Module>, ParseError> {
    let /*mut*/ stream = TokenStream::from_source(source);

    // let _doc_comments = extract_doc_comments(&mut stream);

    let tokens = stream
        .filter(|t| {
            !matches!(
                t.kind,
                TokenKind::EOL | TokenKind::Comment(_) | TokenKind::DocComment(_)
            )
        })
        .chain(iter::once(Token {
            kind: TokenKind::EOF,
            span: source.len()..source.len(),
        }))
        .collect::<Vec<_>>();

    return match_vec!(tokens;

        [
            Token { kind: T![module],                     span: module_token_span },
            Token { kind: TokenKind::LowerIdentifier(id), span: id_token_span },
            remainder @ ..
        ] => {
            Err(ParseError::UnexpectedModuleName {
                message: format!(
                    "Module name must start with a capital letter. Found: `{}`",
                    id
                ),
                span: id_token_span,
            })
        },

        [
            Token { kind: T![module],                            span: module_token_span },
            Token { kind: TokenKind::InvalidUpperIdentifier(id), span: id_token_span },
            remainder @ ..
        ] => {
            Err(ParseError::UnexpectedModuleName {
                message: format!(
                    "Module name must start with a capital letter. Found: `{}`",
                    id
                ),
                span: id_token_span,
            })
        },

        [
            Token { kind: T![module],                     span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
            Token { kind: T![where],                      span: where_token_span },
            remainder @ ..
        ] => {
            let (imports, type_annotations, values, type_definitions) = parse_module_contents(remainder)?;

            Ok(Spanned::from(
                module_token_span,
                where_token_span,
                Module {
                    name: Spanned::from_span(id_token_span, id.to_string()),
                    imports,
                    type_annotations,
                    values,
                    type_definitions,
                },
            ))
        },

        [
            Token { kind: T![module],                     span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
            remainder @ ..
        ] => {
            let remainder = remainder.into_iter().collect::<Vec<_>>();
            let span = find_last_span(&remainder, &id_token_span, source.len());

            Err(ParseError::ExpectedWhereStatement {
                span,
                actual: remainder,
            })
        },

        [
            Token { kind: T![module], span: module_token_span },
            remainder @ ..
        ] => {
            let remainder = remainder.into_iter().collect::<Vec<_>>();
            let span = find_last_span(&remainder, &module_token_span, source.len());

            Err(ParseError::ExpectedModuleName {
                span,
                actual: remainder,
            })
        }
    )
    .map_err(|remaining| ParseError::ExpectedModuleDefinition {
        span: 0..source.len(),
        actual: remaining,
    })
    .and_then(convert::identity);
}

#[must_use]
fn find_last_span(remainder: &[Token], previous_span: &Span, source_length: usize) -> Span {
    remainder
        .iter()
        .filter(|t| !matches!(t.kind, TokenKind::EOF))
        .last()
        .map_or_else(|| previous_span.end..source_length, Token::span)
}

type ModuleContents = (
    Vec<Spanned<Import>>,
    Vec<Spanned<TypeAnnotation>>,
    Vec<Spanned<Value>>,
    Vec<Spanned<TypeDefinition>>,
);
type ParseResult<'a, T> = Result<(Spanned<T>, Vec<Token<'a>>), ParseError<'a>>;

fn parse_module_contents<'a>(
    input: impl Iterator<Item = Token<'a>>,
) -> Result<ModuleContents, ParseError<'a>> {
    let mut imports = vec![];
    let mut type_annotations = vec![];
    let mut values = vec![];
    let mut type_definitions = vec![];

    // let _doc_comments = extract_doc_comments(stream);

    let mut remainder = input.collect::<Vec<_>>();

    while !remainder.is_empty() {
        log::debug!("*parse_module_contents* remainder: {:?}", remainder);

        remainder = match_vec!(remainder;
                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    Token { kind: T![:],                          span: colon_span },
                    remainder @ ..
                ] => {
                    let type_span = id_span.start..colon_span.end;

                    let (t, remainder) = r#type::parse(&type_span, remainder)?;

                    let type_annotation = Spanned {
                        span: type_span.start..t.span.end,
                        value: TypeAnnotation {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            t,
                        },
                    };

                    type_annotations.push(type_annotation);

                    Ok(remainder)
                },

                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    Token { kind: T![=],                          span: eq_span },
                    remainder @ ..
                ] => {
                    let expr_span = id_span.start..eq_span.end;

                    let (expr, remainder) = expr::parse(&expr_span, remainder)?;

                    let value = Spanned {
                        span: expr_span.start..expr.span.end,
                        value: Value {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            expr,
                        },
                    };

                    values.push(value);

                    Ok(remainder)
                },

                [
                    Token { kind: T![import],                 span: import_span },
                    Token { kind: TokenKind::UpperPath(path), span: path_span },
                    remainder @ ..
                ] => {
                    let import = Spanned {
                        span: import_span.start..path_span.end,
                        value: Import {
                            import: Spanned {
                                span: path_span,
                                value: path,
                            }
                        },
                    };

                    imports.push(import);

                    Ok(remainder.collect())
                },

                [
                    Token { kind: T![import],                 span: import_span },
                    Token { kind: TokenKind::LowerPath(path), span: path_span },
                    remainder @ ..
                ] => {
                    let import = Spanned {
                        span: import_span.start..path_span.end,
                        value: Import {
                            import: Spanned {
                                span: path_span,
                                value: path,
                            }
                        },
                    };

                    imports.push(import);

                    Ok(remainder.collect())
                },

                [
                    Token { kind: T![import],                     span: import_span },
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    remainder @ ..
                ] => {
                    let import = Spanned {
                        span: import_span.start..id_span.end,
                        value: Import {
                            import: Spanned {
                                span: id_span,
                                value: NonEmpty::new(id.to_string()),
                            }
                        },
                    };

                    imports.push(import);

                    Ok(remainder.collect())
                },

                [
                    Token { kind: T![import],                        span: import_span },
                    Token { kind: TokenKind::InvalidUpperPath(path), span: path_span },
                    remainder @ ..
                ] => {
                    Err(ParseError::InvalidImport {
                        span: path_span,
                        message: format!(
                            "Import must have a path separated by '::'. Found: `{}`",
                            path
                        ),
                    })
                },

                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    remainder @ ..
                ] => {
                    Err(ParseError::OrphanedIdentifier {
                        span: id_span,
                        name: id.to_string(),
                    })
                },

                [
                    Token { kind: T![typedef],                    span: type_def_span },
                    Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                    Token { kind: T![=],                          span: eq_span },
                    remainder @ ..
                ] => {
                    let type_span = id_span.start..eq_span.end;

                    let (t, remainder) = r#type::parse(&type_span, remainder)?;

                    let type_definition = Spanned {
                        span: type_def_span.start..t.span.end,
                        value: TypeDefinition {
                            t,
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                        }
                    };

                    type_definitions.push(type_definition);

                    Ok(remainder)
                },

                [
                    Token { kind: TokenKind::EOF, span }
                ] => {
                    Ok(Vec::new())
                }
        )
        .map_err(|remaining| ParseError::ExpectedEOF {
            input: vec![],
            remaining,
        })
        .and_then(convert::identity)?;
    }

    Ok((imports, type_annotations, values, type_definitions))
}

//
// parsed AST types
//

type Span = Range<usize>;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    #[must_use]
    fn from_span(span: Span, value: T) -> Self {
        Spanned { span, value }
    }

    #[must_use]
    fn from(start: Span, end: Span, value: T) -> Self {
        Spanned {
            span: start.start..end.end,
            value,
        }
    }

    #[must_use]
    fn span(&self) -> Span {
        self.span.start..self.span.end
    }

    #[must_use]
    fn span_end(self) -> usize {
        self.span.end
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Module {
    name: Spanned<String>,
    imports: Vec<Spanned<Import>>,
    type_annotations: Vec<Spanned<TypeAnnotation>>,
    values: Vec<Spanned<Value>>,
    type_definitions: Vec<Spanned<TypeDefinition>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Import {
    import: Spanned<NonEmpty<String>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeAnnotation {
    name: Spanned<String>,
    t: Spanned<ast::Type>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeDefinition {
    name: Spanned<String>,
    t: Spanned<ast::Type>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Value {
    name: Spanned<String>,
    expr: Spanned<ast::Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError<'a> {
    ExpectedModuleDefinition {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedModuleName {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedWhereStatement {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    UnexpectedModuleName {
        message: String,
        span: Span,
    },
    OrphanedIdentifier {
        span: Span,
        name: String,
    },
    InvalidImport {
        span: Span,
        message: String,
    },
    ExpectedExpr {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedType {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedLambdaReturnType {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedPattern {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedRightArrow {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedPipe {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedClosedParen {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedTupleComma {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedLambdaArgsComma {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedThenKeyWord {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedElseKeyWord {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedEOF {
        input: Vec<Token<'a>>,
        remaining: Vec<Token<'a>>,
    },
}

impl<'a> ParseError<'a> {
    pub fn to_diagnostic<FileId: Clone + std::fmt::Debug>(
        self,
        file_id: FileId,
    ) -> Diagnostic<FileId> {
        match self {
            ParseError::ExpectedModuleDefinition { span, actual: _ } => Diagnostic::error()
                .with_message(
                    "Modules must start with a module definition. Ex: `module HelloWorld where`.",
                )
                .with_code("E0001")
                .with_labels(vec![
                    Label::primary(file_id, span).with_message("expected a module definition")
                ]),
            ParseError::ExpectedModuleName { span, actual: _ } => Diagnostic::error()
                .with_message("`module` must be followed by a name. Ex: `module HelloWorld where`.")
                .with_code("E0002")
                .with_labels(vec![
                    Label::primary(file_id, span).with_message("expected a module name")
                ]),
            ParseError::ExpectedWhereStatement { span, actual: _ } => Diagnostic::error()
                .with_message(
                    "Modules must have a `where` statement. Ex: `module HelloWorld where`.",
                )
                .with_code("E0003")
                .with_labels(vec![
                    Label::primary(file_id, span).with_message("expected a where statement")
                ]),
            ParseError::UnexpectedModuleName { message, span } => Diagnostic::error()
                .with_message(
                    "Module names must start with a capital letter. Ex: `module HelloWorld where`.",
                )
                .with_code("E0004")
                .with_labels(vec![Label::primary(file_id, span).with_message(message)]),
            ParseError::OrphanedIdentifier { span, name } => Diagnostic::error()
                .with_message("Top level identifiers are typically followed by `:` or `=`.")
                .with_code("E0005")
                .with_labels(vec![Label::primary(file_id, span)
                    .with_message(format!("Unexpected identifier '{:?}'", name))]),
            ParseError::InvalidImport { span, message } => Diagnostic::error()
                .with_message("Imports must follow the format 'import path::to::module::values'.")
                .with_code("E0004")
                .with_labels(vec![Label::primary(file_id, span).with_message(message)]),
            ParseError::ExpectedExpr { span, actual } => Diagnostic::error()
                .with_message("Expected expression")
                .with_code("E0006")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected expression, but found: {:?}", t))
                        })
                        .collect(),
                ),
            ParseError::ExpectedType { span, actual } => Diagnostic::error()
                .with_message("Expected type")
                .with_code("E0007")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected type, but found: {:?}", t))
                        })
                        .collect(),
                ),
            ParseError::ExpectedLambdaReturnType { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span)
                            .with_message(format!("Expected return type, but found: {:?}", t.kind))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span).with_message("Expected return type"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message("We encountered an error while parsing a type annotation. We expected to find a return type for your lambda, ex: `Int -> String`")
                    .with_code("E0008")
                    .with_labels(labels)
            }
            ParseError::ExpectedPattern { span, actual } => Diagnostic::error()
                .with_message("Expected pattern")
                .with_code("E0009")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected pattern, but found: {:?}", t))
                        })
                        .collect(),
                ),
            ParseError::ExpectedPipe { span, actual } => Diagnostic::error()
                .with_message("Expected lambda arguments to end with a `|`. Ex: `|arg1, arg2| ->`.")
                .with_code("E0010")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected pipe, but found: {:?}", t))
                        })
                        .collect(),
                ),
            ParseError::ExpectedRightArrow { actual, span } => Diagnostic::error()
                .with_message(
                    "Expected lambda arguments to be followed by `->`. Ex: `|arg1, arg2| ->`.",
                )
                .with_code("E0011")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone()).with_message(format!(
                                "Expected right arrow, but found: {:?}",
                                t.kind
                            ))
                        })
                        .collect(),
                ),
            ParseError::ExpectedClosedParen { span, .. } => {
                let labels =
                    vec![Label::primary(file_id, span).with_message("Expected closed parenthesis")];

                Diagnostic::error()
                    .with_message("Expected a close parenthesis `)`. Ex: `(Int, String)`.")
                    .with_code("E0012")
                    .with_labels(labels)
            }
            ParseError::ExpectedTupleComma { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span)
                            .with_message(format!("Expected a comma, but found: {:?}", t.kind))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span)
                            .with_message("Expected a comma between tuple arguments"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message(
                        "Expected a comma between tuple arguments. Ex: `(Int, String)` instead of `(Int String)`.",
                    )
                    .with_code("E0013")
                    .with_labels(labels)
            }
            ParseError::ExpectedLambdaArgsComma { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span)
                            .with_message(format!("Expected a comma, but found: {:?}", t.kind))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span)
                            .with_message("Expected a comma between lambda arguments"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message(
                        "Expected a comma between lambda arguments. Ex: `|arg1, arg2|` instead of `|arg1 arg2|`.",
                    )
                    .with_code("E0013")
                    .with_labels(labels)
            }
            ParseError::ExpectedThenKeyWord { span, actual } => {
                let labels = actual
                    .first()
                    .map(|t| {
                        Label::primary(file_id.clone(), span).with_message(format!(
                            "Expected 'then' following 'if', but found: {:?}",
                            t.kind
                        ))
                    })
                    .into_iter()
                    .collect();

                Diagnostic::error()
                    .with_message(
                        "Expected 'then' following 'if'. Ex: `if (num > 1) then 1 else 0` instead of `if (num > 1) 1 else 0`.",
                    )
                    .with_code("E0013")
                    .with_labels(labels)
            }
            ParseError::ExpectedElseKeyWord { span, actual } => {
                let labels = actual
                    .first()
                    .map(|t| {
                        Label::primary(file_id.clone(), span).with_message(format!(
                            "Expected 'else' following 'if then', but found: {:?}",
                            t.kind
                        ))
                    })
                    .into_iter()
                    .collect();

                Diagnostic::error()
                    .with_message(
                        "Expected 'else' following 'if then'. Ex: `if (num > 1) then 1 else 0` instead of `if (num > 1) 1 else 0`.",
                    )
                    .with_code("E0013")
                    .with_labels(labels)
            }
            ParseError::ExpectedEOF {
                input: _,
                remaining,
            } => Diagnostic::error()
                .with_message("Expected the file to end")
                .with_code("E0014")
                .with_labels(
                    remaining
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), t.span)
                                .with_message(format!("Unexpected Token {:?}", t.kind))
                        })
                        .collect(),
                ),
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use non_empty_vec::NonEmpty;
    use ordered_float::NotNan;
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;

    use super::*;

    #[test]
    fn test_empty_source() {
        let source = "   \n  ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleDefinition {
            span: 0..6,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 6..6,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_identifier() {
        let source = "module ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 6..7,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 7..7,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_identifier() {
        let source = "module _ ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 7..8,
            actual: vec![
                Token {
                    kind: TokenKind::NilIdentifier,
                    span: 7..8,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 9..9,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_where() {
        let source = "module Test ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 11..12,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 12..12,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_where() {
        let source = "module Test when";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 12..16,
            actual: vec![
                Token {
                    kind: TokenKind::When,
                    span: 12..16,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 16..16,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unexpected_remainder() {
        {
            let source = "module Test where trait";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 23..23,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
        {
            // with comment
            let source = "module Test where trait -- stuff";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 32..32,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
        {
            // with doc comment
            let source = "module Test where trait --! extra stuff";
            let actual = parse(source);

            let expected = Err(ParseError::ExpectedEOF {
                input: vec![],
                remaining: vec![
                    Token {
                        kind: TokenKind::Trait,
                        span: 18..23,
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 39..39,
                    },
                ],
            });

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_empty_module() {
        let source = test_source::EMPTY_MODULE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_lowercase() {
        let source = "module test where";
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `test`".to_string(),
            span: 7..11,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_underscore() {
        let source = "module _Test where";
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `_Test`".to_string(),
            span: 7..12,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_orphaned_identifier() {
        let source = r#"
                module Test
                where

                thing
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::OrphanedIdentifier {
            span: 68..73,
            name: "thing".to_string(),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_partial_value_declaration_with_eq() {
        let source = r#"
            module Test
            where

            thing =
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedExpr {
            span: 56..63,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 72..72,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_int_value_declaration_no_type() {
        let source = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..65,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 64..65,
                            value: ast::Expr::int_literal(0),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    //noinspection DuplicatedCode
    #[test]
    fn test_int_value_declaration_with_type() {
        let source = test_source::INT_VALUE_DECLARATION_WITH_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..67,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        t: Spanned {
                            span: 64..67,
                            value: ast::Type::identifier("Int"),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 80..89,
                    value: Value {
                        name: Spanned {
                            span: 80..85,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 88..89,
                            value: ast::Expr::Literal(ast::LiteralData::Integral(0)),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_float_value_declaration_no_type() {
        let source = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..67,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 64..67,
                            value: ast::Expr::Literal(ast::LiteralData::Fractional(
                                NotNan::new(0.1).unwrap(),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_float_value_declaration_with_type() {
        let source = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..69,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "thing".to_string(),
                        },
                        t: Spanned {
                            span: 64..69,
                            value: ast::Type::identifier("Float"),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 82..93,
                    value: Value {
                        name: Spanned {
                            span: 82..87,
                            value: "thing".to_string(),
                        },
                        expr: Spanned {
                            span: 90..93,
                            value: ast::Expr::Literal(ast::LiteralData::Fractional(
                                NotNan::new(0.1).unwrap(),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_function_type_annotation() {
        let source = r#"
            module Test
            where

            incomplete : Int -> 0
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> =
            Err(ParseError::ExpectedLambdaReturnType {
                span: 69..75,
                actual: vec![
                    Token {
                        span: 76..77,
                        kind: TokenKind::LiteralInt(0),
                    },
                    Token {
                        kind: TokenKind::EOF,
                        span: 86..86,
                    },
                ],
            });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_parens_unit_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : ("#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedClosedParen {
            span: 74..75,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 75..75,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_parens_tuple_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : (Int, String
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedClosedParen {
            span: 74..95,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 95..95,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_comma_tuple_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : (Int String)
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedTupleComma {
            span: 74..78,
            actual: vec![
                Token {
                    kind: TokenKind::UpperIdentifier("String"),
                    span: 79..85,
                },
                Token {
                    kind: TokenKind::CloseParen,
                    span: 85..86,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 95..95,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parens_expression() {
        let source = r#"
            module Test
            where

            parens = (1 + 2)
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..72,
                    value: Value {
                        name: Spanned {
                            span: 56..62,
                            value: "parens".to_string(),
                        },
                        expr: Spanned {
                            span: 65..72,
                            value: ast::Expr::paren(ast::Expr::bin_op(
                                "+",
                                ast::Expr::int_literal(1),
                                ast::Expr::int_literal(2),
                            )),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tuple_expression() {
        let source = r#"
            module Test
            where

            tuple = (1 + 2, 3)
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..74,
                    value: Value {
                        name: Spanned {
                            span: 56..61,
                            value: "tuple".to_string(),
                        },
                        expr: Spanned {
                            span: 64..74,
                            value: ast::Expr::tuple(vec![
                                ast::Expr::bin_op(
                                    "+",
                                    ast::Expr::int_literal(1),
                                    ast::Expr::int_literal(2),
                                ),
                                ast::Expr::int_literal(3),
                            ]),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_pipe_lambda_expr() {
        let source = r#"
            module Test
            where

            no_right_arrow = |arg -> arg
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedPipe {
            span: 56..77,
            actual: vec![
                Token {
                    kind: TokenKind::LowerIdentifier("arg"),
                    span: 74..77,
                },
                Token {
                    kind: TokenKind::RightArrow,
                    span: 78..80,
                },
                Token {
                    kind: TokenKind::LowerIdentifier("arg"),
                    span: 81..84,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 93..93,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_right_arrow_lambda_expr() {
        let source = r#"
            module Test
            where

            no_right_arrow = |arg| arg
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedRightArrow {
            span: 73..78,
            actual: vec![Token {
                kind: TokenKind::LowerIdentifier("arg"),
                span: 74..77,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_body_lambda_expr() {
        let source = r#"
            module Test
            where

            no_body = |arg| ->
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedExpr {
            span: 66..67,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 83..83,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_comma_lambda_expr() {
        let source = r#"
            module Test
            where

            no_body = |arg1 arg2| -> arg1
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedLambdaArgsComma {
            span: 66..77,
            actual: vec![Token {
                kind: TokenKind::LowerIdentifier("arg2"),
                span: 72..76,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_single_arg_function_declaration_with_type() {
        let source = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 15..44,
            value: Module {
                name: Spanned {
                    span: 22..26,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 58..89,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 58..76,
                            value: "increment_positive".to_string(),
                        },
                        t: Spanned {
                            span: 79..89,
                            value: ast::Type::lambda(
                                ast::Type::identifier("Int"),
                                ast::Type::identifier("Int"),
                            ),
                        },
                    },
                }],
                values: vec![
                    Spanned {
                        span: 102..131,
                        value: Value {
                            name: Spanned {
                                span: 102..120,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 123..131,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::int_literal(0)],
                                    ast::Expr::int_literal(0),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 144..177,
                        value: Value {
                            name: Spanned {
                                span: 144..162,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 165..177,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("x")],
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("x"),
                                        ast::Expr::int_literal(1),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_multi_arg_function_declaration_with_type() {
        let source = r#"


            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, "")| -> 0
            increment_by_length = |(x, y)| -> x + 1
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 15..44,
            value: Module {
                name: Spanned {
                    span: 22..26,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 58..100,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 58..77,
                            value: "increment_by_length".to_string(),
                        },
                        t: Spanned {
                            span: 80..100,
                            value: ast::Type::lambda(
                                ast::Type::tuple(vec![
                                    ast::Type::identifier("Int"),
                                    ast::Type::identifier("String"),
                                ]),
                                ast::Type::identifier("Int"),
                            ),
                        },
                    },
                }],
                values: vec![
                    Spanned {
                        span: 113..149,
                        value: Value {
                            name: Spanned {
                                span: 113..132,
                                value: "increment_by_length".to_string(),
                            },
                            expr: Spanned {
                                span: 135..149,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::tuple(vec![
                                        ast::Pattern::int_literal(0),
                                        ast::Pattern::string_literal(r#""""#),
                                    ])],
                                    ast::Expr::int_literal(0),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 162..201,
                        value: Value {
                            name: Spanned {
                                span: 162..181,
                                value: "increment_by_length".to_string(),
                            },
                            expr: Spanned {
                                span: 184..201,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::tuple(vec![
                                        ast::Pattern::identifier("x"),
                                        ast::Pattern::identifier("y"),
                                    ])],
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("x"),
                                        ast::Expr::int_literal(1),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_curried_function_declaration_with_type() {
        let source = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..96,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..61,
                            value: "apply".to_string(),
                        },
                        t: Spanned {
                            span: 64..96,
                            value: ast::Type::lambda(
                                ast::Type::lambda(
                                    ast::Type::identifier("String"),
                                    ast::Type::identifier("Int"),
                                ),
                                ast::Type::lambda(
                                    ast::Type::identifier("String"),
                                    ast::Type::identifier("Int"),
                                ),
                            ),
                        },
                    },
                }],
                values: vec![Spanned {
                    span: 109..143,
                    value: Value {
                        name: Spanned {
                            span: 109..114,
                            value: "apply".to_string(),
                        },
                        expr: Spanned {
                            span: 117..143,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::identifier("f")],
                                ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("value")],
                                    ast::Expr::application(
                                        NonEmpty::new("f"),
                                        vec![ast::Expr::identifier("value")],
                                    ),
                                ),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_type_annotation() {
        let source = r#"
            module Test
            where

            unit_type : ()"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..70,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..65,
                            value: "unit_type".to_string(),
                        },
                        t: Spanned {
                            span: 68..70,
                            value: ast::Type::Unit,
                        },
                    },
                }],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_expr_annotation() {
        let source = r#"
            module Test
            where

            unit_expr = ()"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..70,
                    value: Value {
                        name: Spanned {
                            span: 56..65,
                            value: "unit_expr".to_string(),
                        },
                        expr: Spanned {
                            span: 68..70,
                            value: ast::Expr::Unit,
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unit_pattern_annotation() {
        let source = r#"
            module Test
            where

            unit_pattern = |()| -> 0"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..80,
                    value: Value {
                        name: Spanned {
                            span: 56..68,
                            value: "unit_pattern".to_string(),
                        },
                        expr: Spanned {
                            span: 71..80,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::Unit],
                                ast::Expr::int_literal(0),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parens_type_annotation() {
        let source = r#"
            module Test
            where

            single_parens_type : (Int)"#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..82,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..74,
                            value: "single_parens_type".to_string(),
                        },
                        t: Spanned {
                            span: 77..82,
                            value: ast::Type::identifier("Int"),
                        },
                    },
                }],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_many_parens() {
        let source = r#"
            module Test
            where

            many_parens_type : ((((), (Int))))"#;
        let actual: Result<Spanned<Module>, ParseError> = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![Spanned {
                    span: 56..90,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 56..72,
                            value: "many_parens_type".to_string(),
                        },
                        t: Spanned {
                            span: 75..90,
                            value: ast::Type::tuple(vec![
                                ast::Type::Unit,
                                ast::Type::identifier("Int"),
                            ]),
                        },
                    },
                }],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_if_then_else() {
        let source = test_source::SIMPLE_IF_THEN_ELSE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![
                    Spanned {
                        span: 56..178,
                        value: Value {
                            name: Spanned {
                                span: 56..74,
                                value: "increment_positive".to_string(),
                            },
                            expr: Spanned {
                                span: 77..178,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("num")],
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_positive?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "+",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            },
                        },
                    },
                    Spanned {
                        span: 191..313,
                        value: Value {
                            name: Spanned {
                                span: 191..209,
                                value: "decrement_negative".to_string(),
                            },
                            expr: Spanned {
                                span: 212..313,
                                value: ast::Expr::lambda(
                                    vec![ast::Pattern::identifier("num")],
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_negative?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "-",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            },
                        },
                    },
                ],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_if_with_no_then_else() {
        let source = r#"
            module Test
            where

            missing_then = |num| ->
              if Number::is_positive?(num) num + 1
              else num
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedThenKeyWord {
            span: 123..126,
            actual: vec![
                Token {
                    kind: TokenKind::LowerIdentifier("num"),
                    span: 123..126,
                },
                Token {
                    kind: TokenKind::Plus,
                    span: 127..128,
                },
                Token {
                    kind: TokenKind::LiteralInt(1),
                    span: 129..130,
                },
                Token {
                    kind: TokenKind::Else,
                    span: 145..149,
                },
                Token {
                    kind: TokenKind::LowerIdentifier("num"),
                    span: 150..153,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 162..162,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_if_then_with_no_else() {
        let source = r#"
            module Test
            where

            missing_then = |num| ->
              if Number::is_positive?(num) then num + 1
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedElseKeyWord {
            span: 144..144,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 144..144,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_nested_if_then_else() {
        let source = test_source::NESTED_IF_THEN_ELSE;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![Spanned {
                    span: 56..277,
                    value: Value {
                        name: Spanned {
                            span: 56..78,
                            value: "increment_or_decrement".to_string(),
                        },
                        expr: Spanned {
                            span: 81..277,
                            value: ast::Expr::lambda(
                                vec![ast::Pattern::identifier("num")],
                                ast::Expr::if_then_else(
                                    ast::Expr::application(
                                        NonEmpty::from((vec!["Number"], "is_positive?")),
                                        vec![ast::Expr::identifier("num")],
                                    ),
                                    ast::Expr::bin_op(
                                        "+",
                                        ast::Expr::identifier("num"),
                                        ast::Expr::int_literal(1),
                                    ),
                                    ast::Expr::if_then_else(
                                        ast::Expr::application(
                                            NonEmpty::from((vec!["Number"], "is_negative?")),
                                            vec![ast::Expr::identifier("num")],
                                        ),
                                        ast::Expr::bin_op(
                                            "-",
                                            ast::Expr::identifier("num"),
                                            ast::Expr::int_literal(1),
                                        ),
                                        ast::Expr::identifier("num"),
                                    ),
                                ),
                            ),
                        },
                    },
                }],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_imports() {
        let source = r#"
            module Test
            where

            import std::pretty::long::import::path
            import std::bool::Bool
            import std::bool::not
            import std::function::(<|)
            import std
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![
                    spanned_import(56, 94, vec!["std", "pretty", "long", "import", "path"]),
                    spanned_import(107, 129, vec!["std", "bool", "Bool"]),
                    spanned_import(142, 163, vec!["std", "bool", "not"]),
                    spanned_import(176, 202, vec!["std", "function", "(<|)"]),
                    spanned_import(215, 225, vec!["std"]),
                ],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_import() {
        let source = r#"
            module Test
            where

            import std::pretty::long::unfinished::
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::InvalidImport {
            span: 63..94,
            message: "Import must have a path separated by '::'. Found: `std::pretty::long::unfinished::`".to_string(),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_typedef() {
        let source = r#"
            module Test
            where

            typedef Name = String
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![Spanned {
                    span: 56..77,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..68,
                            value: "Name".to_string(),
                        },
                        t: Spanned {
                            span: 71..77,
                            value: ast::Type::identifier("String"),
                        },
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_typedef_2_union() {
        let source = r#"
            module Bool
            where

            typedef Bool =
              | False
              | True
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Bool".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![Spanned {
                    span: 56..113,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..68,
                            value: "Bool".to_string(),
                        },
                        t: Spanned {
                            span: 87..113,
                            value: ast::Type::union(vec![
                                ast::Type::identifier("False"),
                                ast::Type::identifier("True"),
                            ]),
                        },
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simple_typedef_3_union() {
        let source = r#"
            module Test
            where

            typedef Thing =
              | This
              | That
              | TheOther
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![Spanned {
                    span: 56..138,
                    value: TypeDefinition {
                        name: Spanned {
                            span: 64..69,
                            value: "Thing".to_string(),
                        },
                        t: Spanned {
                            span: 88..138,
                            value: ast::Type::union(vec![
                                ast::Type::identifier("This"),
                                ast::Type::identifier("That"),
                                ast::Type::identifier("TheOther"),
                            ]),
                        },
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_union() {
        let source = r#"
            module Bool
            where

            typedef IncompleteUnion = False |
        "#;
        let actual = parse(source);

        let expected: Result<Spanned<Module>, ParseError> = Err(ParseError::ExpectedTypeName {
            span: 88..89,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 98..98,
            }],
        });

        assert_eq!(expected, actual);
    }

    //
    // #[test]
    // fn test_multi_property_union_type() {
    //     {
    //         let source = test_source::MULTI_PROPERTY_UNION_TYPE_1;
    //
    //         assert_no_errors(source);
    //     }
    //     {
    //         let source = test_source::MULTI_PROPERTY_UNION_TYPE_2;
    //
    //         assert_no_errors(source);
    //     }
    //     {
    //         let source = test_source::MULTI_PROPERTY_UNION_TYPE_3;
    //
    //         assert_no_errors(source);
    //     }
    // }

    fn spanned_import(start: usize, end: usize, segments: Vec<&str>) -> Spanned<Import> {
        Spanned {
            span: start..end,
            value: Import {
                import: Spanned {
                    span: (start + 7)..end,
                    value: unsafe {
                        NonEmpty::new_unchecked(
                            segments.into_iter().map(ToString::to_string).collect(),
                        )
                    },
                },
            },
        }
    }
}
