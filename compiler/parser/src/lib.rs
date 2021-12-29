use improved_slice_patterns::match_vec;
use itertools::Itertools;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, TokenStream};

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
            Token { kind: TokenKind::Module,         span: module_token_span },
        ] => {
            Err(ParseError::ExpectedModuleName {
                span: module_token_span,
                actual: None,
            })
        },

        [
            Token { kind: TokenKind::Module,         span: module_token_span },
            Token { kind: TokenKind::Identifier(id), span: id_token_span },
        ] => {
            Err(ParseError::ExpectedWhereStatement {
                span: module_token_span.start..id_token_span.end,
                actual: None,
            })
        },

        [
            Token { kind: TokenKind::Module, span: module_token_span },
            Token { kind,                    span: not_id_token_span }
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
            Token { kind: TokenKind::Module,         span: module_token_span },
            Token { kind: TokenKind::Identifier(id), span: id_token_span },
            Token { kind: TokenKind::Where,          span: where_token_span },
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
            Token { kind: TokenKind::Module,         span: module_token_span },
            Token { kind: TokenKind::Identifier(id), span: id_token_span },
            Token { kind: TokenKind::Where,          span: where_token_span },
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
            Token { kind: TokenKind::Module,         span: module_token_span },
            Token { kind: TokenKind::Identifier(id), span: id_token_span },
            Token { kind,                            span: not_where_token_span },
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
            ParseError::ExpectedEOF { actual: remaining }
        }
    })
    .and_then(|s| s)
    .and_then(validate_module);
}

fn validate_module<'a>(module: Spanned<Module>) -> Result<Spanned<Module>, ParseError<'a>> {
    let id = &module.value.name.value;
    let id_span = &module.value.name.span;

    return match id.chars().next() {
        Some(first) if first.is_ascii_uppercase() => Ok(module),
        Some(_) => Err(ParseError::UnexpectedModuleName {
            message: format!(
                "Module name must start with a capital letter. Found: `{}`",
                id
            ),
            span: id_span.clone(),
        }),
        None => Err(ParseError::UnexpectedModuleName {
            message: "Module name must not be empty".to_string(),
            span: id_span.clone(),
        }),
    };
}

fn parse_module_contents<'a>(
    tokens: impl Iterator<Item = Token<'a>>,
) -> Result<(Vec<Spanned<TypeAnnotation>>, Vec<Spanned<Value>>), ParseError<'a>> {
    let mut type_annotations = vec![];
    let mut values = vec![];

    // let _doc_comments = extract_doc_comments(stream);

    let mut remainder = tokens.collect::<Vec<_>>();

    loop {
        remainder = match_vec!(remainder;
                [
                    Token { kind: TokenKind::Identifier(id), span: id_span },
                ] => {
                    Err(ParseError::OrphanedIdentifier {
                        span: id_span,
                        name: id.to_string(),
                    })
                },

                [
                    Token { kind: TokenKind::Identifier(id), span: id_span },
                    Token { kind: TokenKind::Colon,          span: colon_span },
                    remainder @ ..
                ] => {
                    let type_span = id_span.start..colon_span.end;

                    let (t, tokens) = parse_type(&type_span, remainder)?;

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
                    Token { kind: TokenKind::Identifier(id), span: id_span },
                    Token { kind: TokenKind::Eq,             span: eq_span },
                    remainder @ ..
                ] => {
                    let expr_span = id_span.start..eq_span.end;

                    let (expr, tokens) = parse_expr(&expr_span, remainder)?;

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
        .map_err(|remaining| ParseError::ExpectedEOF { actual: remaining })
        .and_then(|s| s)?;

        if remainder.is_empty() {
            break;
        }
    }

    Ok((type_annotations, values))
}

fn parse_type<'a>(
    type_span: &Span,
    tokens: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Type>, Vec<Token<'a>>), ParseError<'a>> {
    // let _doc_comments = extract_doc_comments(stream);

    match_vec!(tokens.collect::<Vec<_>>();
        [
            Token { kind: TokenKind::Identifier(id), span },
            remainder @ ..,
        ] => Ok((Spanned {
            span,
            value: ast::Type::Identifier(id.to_string()),
        }, remainder.collect())),

        [..] => Err(ParseError::ExpectedExpr {
            span: type_span.clone(),
            actual: None,
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF { actual: remaining })
    .and_then(|s| s)
}

fn parse_expr<'a>(
    expr_span: &Span,
    tokens: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Expr>, Vec<Token<'a>>), ParseError<'a>> {
    // let _doc_comments = extract_doc_comments(stream);

    match_vec!(tokens.collect::<Vec<_>>();
        [
            Token { kind: TokenKind::LiteralInt(i), span },
        ] => Ok((Spanned {
            span,
            value: ast::Expr::Literal(ast::LiteralData::Integral(i)),
        }, Vec::new())),

        [
            Token { kind: TokenKind::LiteralFloat(f), span },
        ] => Ok((Spanned {
            span,
            value: ast::Expr::Literal(ast::LiteralData::fractional(f)),
        }, Vec::new())),

        [
            Token { kind: TokenKind::LiteralInt(i), span },
            remainder @ ..
        ] => Ok((Spanned {
            span,
            value: ast::Expr::Literal(ast::LiteralData::Integral(i)),
        }, remainder.collect())),

        [..] => Err(ParseError::ExpectedExpr {
            span: expr_span.clone(),
            actual: None,
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF { actual: remaining })
    .and_then(|s| s)
}

#[must_use]
fn extract_doc_comments<'a>(stream: &'a mut TokenStream<'a>) -> Vec<Token<'a>> {
    stream
        .take_while_ref(|t| match t.kind {
            TokenKind::EOL /*| TokenKind::Comment(_) | TokenKind::DocComment(_)*/ => true,
            _ => false,
        })
        .collect::<Vec<_>>()
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
        actual: Option<TokenKind<'a>>,
    },
    ExpectedEOF {
        actual: Vec<Token<'a>>,
    },
}

impl<'a> ParseError<'a> {
    pub fn to_diagnostic<FileId: Clone>(self, file_id: FileId) -> Diagnostic<FileId> {
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
                .with_labels(vec![actual.map_or_else(
                    || {
                        Label::primary(file_id.clone(), span.clone())
                            .with_message("Expected expression".to_string())
                    },
                    |t| {
                        Label::primary(file_id.clone(), span.clone())
                            .with_message(format!("Expected expression, but found: {:?}", t))
                    },
                )]),
            ParseError::ExpectedEOF { actual } => Diagnostic::error()
                .with_message("Expected the file to end")
                .with_code("E0007")
                .with_labels(
                    actual
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
    use alloy_ast as ast;
    use ordered_float::NotNan;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_empty_source() {
        let source: &str = "   \n  ";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleDefinition {
            span: 0..6,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_identifier() {
        let source: &str = "module";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 0..6,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_identifier() {
        let source: &str = "module _";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedModuleName {
            span: 0..8,
            actual: Some(TokenKind::NilIdentifier),
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_where() {
        let source: &str = "module Test";
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedWhereStatement {
            span: 0..11,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_where() {
        let source: &str = "module Test when";
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
            actual: vec![Token {
                kind: TokenKind::Trait,
                span: 18..23,
            }],
        });

        {
            let source: &str = "module Test where trait";
            let actual = parse(source);

            assert_eq!(expected, actual);
        }
        {
            // with comment
            let source: &str = "module Test where trait -- stuff";
            let actual = parse(source);

            assert_eq!(expected, actual);
        }
        {
            // with doc comment
            let source: &str = "module Test where trait --! extra stuff";
            let actual = parse(source);

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
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
        let source: &str = test_source::EMPTY_MODULE_LOWERCASE;
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `test`".to_string(),
            span: 20..24,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_underscore() {
        let source: &str = test_source::EMPTY_MODULE_UNDERSCORE;
        let actual = parse(source);

        let expected = Err(ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `_Test`".to_string(),
            span: 20..25,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_orphaned_identifier() {
        let source: &str = r#"
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
        let source: &str = r#"
            module Test
            where

            thing =
"#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedExpr {
            span: 56..63,
            actual: None,
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_int_value_declaration_no_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
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
                            value: ast::Expr::Literal(ast::LiteralData::Integral(0)),
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
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_TYPE;
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
                            value: ast::Type::Identifier("Int".to_string()),
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
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;
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
        let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;
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
                            value: ast::Type::Identifier("Float".to_string()),
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

    // #[test]
    // fn test_single_arg_function_declaration_with_type() {
    //     let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;
    //     let actual = parse(source);
    //
    //     let expected = Ok(Spanned {
    //         span: 13..42,
    //         value: Module {
    //             name: Spanned {
    //                 span: 20..24,
    //                 value: "Test".to_string(),
    //             },
    //             type_annotations: vec![
    //                 Spanned {
    //                     span: 56..69,
    //                     value: TypeAnnotation {
    //                         name: Spanned {
    //                             span: 56..61,
    //                             value: "thing".to_string(),
    //                         },
    //                         t: Spanned {
    //                             span: 64..69,
    //                             value: ast::Type::Identifier("Float".to_string()),
    //                         },
    //                     },
    //                 },
    //                 Spanned {
    //                     span: 56..69,
    //                     value: TypeAnnotation {
    //                         name: Spanned {
    //                             span: 56..61,
    //                             value: "thing".to_string(),
    //                         },
    //                         t: Spanned {
    //                             span: 64..69,
    //                             value: ast::Type::Identifier("Int".to_string()),
    //                         },
    //                     },
    //                 },
    //             ],
    //             values: vec![Spanned {
    //                 span: 82..93,
    //                 value: Value {
    //                     name: Spanned {
    //                         span: 82..87,
    //                         value: "thing".to_string(),
    //                     },
    //                     expr: Spanned {
    //                         span: 90..93,
    //                         value: ast::Expr::Literal(ast::LiteralData::Fractional(
    //                             NotNan::new(0.1).unwrap(),
    //                         )),
    //                     },
    //                 },
    //             }],
    //         },
    //     });
    //
    //     assert_eq!(expected, actual);
    // }
    //
    // #[test]
    // fn test_multi_arg_function_declaration_with_type() {
    //     let source: &str = test_source::MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_curried_function_declaration_with_type() {
    //     let source: &str = test_source::CURRIED_FUNCTION_DECLARATION_WITH_TYPE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_simple_if_then_else() {
    //     let source: &str = test_source::SIMPLE_IF_THEN_ELSE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_nested_if_then_else() {
    //     let source: &str = test_source::NESTED_IF_THEN_ELSE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_multi_property_union_type() {
    //     {
    //         let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_1;
    //
    //         assert_no_errors(source);
    //     }
    //     {
    //         let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_2;
    //
    //         assert_no_errors(source);
    //     }
    //     {
    //         let source: &str = test_source::MULTI_PROPERTY_UNION_TYPE_3;
    //
    //         assert_no_errors(source);
    //     }
    // }
}
