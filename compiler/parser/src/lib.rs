use core::convert;
use std::iter;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, TokenStream};

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
        .clone()
        .filter(|t| {
            !matches!(
                t.kind,
                TokenKind::EOL | TokenKind::Comment(_) | TokenKind::DocComment(_)
            )
        })
        .collect::<Vec<_>>();

    return match_vec!(tokens;
        [
            Token { kind: TokenKind::Module,              span: module_token_span },
        ] => {
            Err(ParseError::ExpectedModuleName {
                span: module_token_span,
                actual: None,
            })
        },

        [
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
        ] => {
            Err(ParseError::ExpectedWhereStatement {
                span: module_token_span.start..id_token_span.end,
                actual: None,
            })
        },

        [
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind: TokenKind::LowerIdentifier(id), span: id_token_span },
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
            Token { kind: TokenKind::Module,              span: module_token_span },
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
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind,                                 span: not_id_token_span }
        ] => {
            Err(ParseError::ExpectedModuleName {
                span: module_token_span.start..not_id_token_span.end,
                actual: Some(kind),
            })
        },

        //
        // TODO: combine this with the one below
        //
        [
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
            Token { kind: TokenKind::Where,               span: where_token_span },
        ] => {
            Ok(Spanned::from(
                module_token_span,
                where_token_span,
                Module {
                    name: Spanned::from_span(id_token_span, id.to_string()),
                    type_annotations: vec![],
                    values: vec![],
                },
            ))
        },

        [
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
            Token { kind: TokenKind::Where,               span: where_token_span },
            remainder @ ..
        ] => {
            let (type_annotations, values) = parse_module_contents(remainder)?;

            Ok(Spanned::from(
                module_token_span,
                where_token_span,
                Module {
                    name: Spanned::from_span(id_token_span, id.to_string()),
                    type_annotations,
                    values,
                },
            ))
        },

        [
            Token { kind: TokenKind::Module,              span: module_token_span },
            Token { kind: TokenKind::UpperIdentifier(id), span: id_token_span },
            Token { kind,                                 span: not_where_token_span },
        ] => {
            Err(ParseError::ExpectedWhereStatement {
                span: not_where_token_span,
                actual: Some(kind),
            })
        },
    )
    .map_err(|remaining| {
        if remaining.is_empty() {
            ParseError::ExpectedModuleDefinition {
                span: 0..source.len(),
                actual: None,
            }
        } else {
            ParseError::ExpectedEOF {
                input: vec![],
                remaining,
            }
        }
    })
    .and_then(convert::identity);
}

type ModuleContents = (Vec<Spanned<TypeAnnotation>>, Vec<Spanned<Value>>);
type ParseResult<'a, T> = Result<(Spanned<T>, Vec<Token<'a>>), ParseError<'a>>;

fn parse_module_contents<'a>(
    input: impl Iterator<Item = Token<'a>>,
) -> Result<ModuleContents, ParseError<'a>> {
    let mut type_annotations = vec![];
    let mut values = vec![];

    // let _doc_comments = extract_doc_comments(stream);

    let mut remainder = input.collect::<Vec<_>>();

    while !remainder.is_empty() {
        log::debug!("*parse_module_contents* remainder: {:?}", remainder);

        remainder = match_vec!(remainder;
                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                ] => {
                    Err(ParseError::OrphanedIdentifier {
                        span: id_span,
                        name: id.to_string(),
                    })
                },

                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    Token { kind: TokenKind::Colon,               span: colon_span },
                    remainder @ ..
                ] => {
                    let type_span = id_span.start..colon_span.end;

                    let (t, tokens) = r#type::parse(&type_span, remainder)?;

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

                    Ok(tokens)
                },

                [
                    Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                    Token { kind: TokenKind::Eq,                  span: eq_span },
                    remainder @ ..
                ] => {
                    let expr_span = id_span.start..eq_span.end;

                    let (expr, tokens) = expr::parse(&expr_span, remainder)?;

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

                    Ok(tokens)
                }
        )
        .map_err(|remaining| ParseError::ExpectedEOF {
            input: vec![],
            remaining,
        })
        .and_then(convert::identity)?;
    }

    Ok((type_annotations, values))
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
    fn from_span(span: Span, value: T) -> Self {
        Spanned { span, value }
    }

    fn from(start: Span, end: Span, value: T) -> Self {
        Spanned {
            span: start.start..end.end,
            value,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Module {
    name: Spanned<String>,
    type_annotations: Vec<Spanned<TypeAnnotation>>,
    values: Vec<Spanned<Value>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeAnnotation {
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
        actual: Option<TokenKind<'a>>,
    },
    ExpectedModuleName {
        span: Span,
        actual: Option<TokenKind<'a>>,
    },
    ExpectedWhereStatement {
        span: Span,
        actual: Option<TokenKind<'a>>,
    },
    UnexpectedModuleName {
        message: String,
        span: Span,
    },
    OrphanedIdentifier {
        span: Span,
        name: String,
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
                dbg!(&actual);

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

                dbg!(&labels);

                Diagnostic::error()
                    .with_message("Expected lambda to have a return type. Ex `Int -> String`")
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
            ParseError::ExpectedClosedParen { span, .. } => {
                let labels = vec![Label::primary(file_id.clone(), span)
                    .with_message("Expected closed parenthesis")];

                Diagnostic::error()
                    .with_message("Expected a close parenthesis `)`. Ex: `(Int, String)`.")
                    .with_code("E0011")
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
                    .with_code("E0012")
                    .with_labels(labels)
            }
            ParseError::ExpectedEOF {
                input: _,
                remaining,
            } => Diagnostic::error()
                .with_message("Expected the file to end")
                .with_code("E0013")
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
mod tests {
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
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_identifier() {
        let source = "module";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 0..6,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_identifier() {
        let source = "module _";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 0..8,
            actual: Some(TokenKind::NilIdentifier),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_where() {
        let source = "module Test";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 0..11,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_where() {
        let source = "module Test when";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 12..16,
            actual: Some(TokenKind::When),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unexpected_remainder() {
        let expected = Err(ParseError::ExpectedEOF {
            input: vec![],
            remaining: vec![Token {
                kind: TokenKind::Trait,
                span: 18..23,
            }],
        });

        {
            let source = "module Test where trait";
            let actual = parse(source);

            assert_eq!(expected, actual);
        }
        {
            // with comment
            let source = "module Test where trait -- stuff";
            let actual = parse(source);

            assert_eq!(expected, actual);
        }
        {
            // with doc comment
            let source = "module Test where trait --! extra stuff";
            let actual = parse(source);

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
                type_annotations: vec![],
                values: vec![],
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
            actual: Vec::new(),
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
                actual: vec![Token {
                    span: 76..77,
                    kind: TokenKind::LiteralInt(0),
                }],
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
            actual: vec![],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_no_close_parens_tuple_type_annotation() {
        let source = r#"
            module Test
            where

            no_close_parens : (Int
            0
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedClosedParen {
            span: 74..78,
            actual: vec![
                Token {
                    kind: TokenKind::UpperIdentifier("Int"),
                    span: 75..78,
                },
                Token {
                    kind: TokenKind::LiteralInt(0),
                    span: 91..92,
                },
            ],
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
            span: 74..86,
            actual: vec![Token {
                kind: TokenKind::UpperIdentifier("String"),
                span: 79..85,
            }],
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
            },
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
            },
        });

        assert_eq!(expected, actual);
    }

    //
    // #[test]
    // fn test_simple_if_then_else() {
    //     let source = test_source::SIMPLE_IF_THEN_ELSE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_nested_if_then_else() {
    //     let source = test_source::NESTED_IF_THEN_ELSE;
    //
    //     assert_no_errors(source)
    // }
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
}
