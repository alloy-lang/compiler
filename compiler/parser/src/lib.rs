use itertools::Itertools;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use alloy_ast as ast;
use alloy_ast::Expr;
use alloy_lexer::{Token, TokenKind, TokenStream};

//
// parse functions
//

type PTokenStream<'a> = PeekMoreIterator<TokenStream<'a>>;

pub fn parse(source: &str) -> Result<Spanned<Module>, ParseError> {
    let mut stream = TokenStream::from_source(source).peekmore();

    let _doc_comments = extract_doc_comments(&mut stream);

    while let Some(token) = stream.next() {
        match token.kind {
            TokenKind::Module => {
                return parse_module(token.span, &mut stream)
                    .and_then(validate_module)
                    .and_then(|module| validate_eof(module, &mut stream));
            }
            _ => todo!("Unhandled token kind"),
        }
    }

    Err(ParseError::ExpectedModuleDefinition {
        span: 0..source.len(),
        actual: None,
    })
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

fn validate_eof<'a>(
    module: Spanned<Module>,
    stream: &mut PTokenStream<'a>,
) -> Result<Spanned<Module>, ParseError<'a>> {
    let remaining = stream
        .filter(|t| match t.kind {
            TokenKind::EOL | TokenKind::Comment(_) | TokenKind::DocComment(_) => false,
            _ => true,
        })
        .collect::<Vec<_>>();

    if remaining.is_empty() {
        Ok(module)
    } else {
        Err(ParseError::ExpectedEOF { actual: remaining })
    }
}

fn parse_module<'a>(
    module_token_span: Span,
    stream: &mut PTokenStream<'a>,
) -> Result<Spanned<Module>, ParseError<'a>> {
    let _doc_comments = extract_doc_comments(stream);

    match stream.next() {
        Some(Token {
            kind: TokenKind::Identifier(id),
            span: id_token_span,
        }) => {
            let _doc_comments = extract_doc_comments(stream);

            match stream.next() {
                Some(Token {
                    kind: TokenKind::Where,
                    span: where_token_span,
                }) => {
                    let (values) = parse_module_contents(stream)?;

                    Ok(Spanned::from(
                        module_token_span,
                        where_token_span,
                        Module {
                            name: Spanned::from_span(id_token_span, id.to_string()),
                            values,
                        },
                    ))
                }
                Some(not_where_token) => Err(ParseError::ExpectedWhereStatement {
                    span: not_where_token.span,
                    actual: Some(not_where_token.kind),
                }),
                None => Err(ParseError::ExpectedWhereStatement {
                    span: module_token_span.start..id_token_span.end,
                    actual: None,
                }),
            }
        }
        Some(not_id_token) => Err(ParseError::ExpectedModuleName {
            span: module_token_span.start..not_id_token.span.end,
            actual: Some(not_id_token.kind),
        }),
        None => Err(ParseError::ExpectedModuleName {
            span: module_token_span,
            actual: None,
        }),
    }
}

fn parse_module_contents<'a>(
    stream: &mut PTokenStream<'a>,
) -> Result<(Vec<Spanned<Value>>), ParseError<'a>> {
    let mut values = vec![];

    let _doc_comments = extract_doc_comments(stream);

    match stream.peek().cloned() {
        Some(Token {
            kind: TokenKind::Identifier(id),
            span: id_span,
        }) => match stream.peek_next() {
            Some(Token {
                kind: TokenKind::Eq,
                span: eq_span,
            }) => {
                let expr_span = id_span.start..eq_span.clone().end;

                stream.nth(1);

                match parse_expr(expr_span, stream) {
                    Ok(expr) => values.push(Spanned {
                        span: id_span.start..expr.span.end,
                        value: Value {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            expr,
                        },
                    }),
                    Err(err) => {
                        stream.reset_cursor();
                        return Err(err);
                    }
                }
            }
            _ => stream.reset_cursor(),
        },
        Some(Token {
            kind: TokenKind::Trait,
            span: _,
        }) => return Err(ParseError::ExpectedEOF { actual: vec![] }),
        Some(t) => todo!("Unhandled token kind: {:?}", t),
        None => {
            // skip if stream is empty
        }
    }

    Ok((values))
}

fn parse_expr<'a>(
    expr_span: Span,
    stream: &mut PTokenStream<'a>,
) -> Result<Spanned<ast::Expr>, ParseError<'a>> {
    let _doc_comments = extract_doc_comments(stream);

    return match stream.next() {
        Some(Token {
            kind: TokenKind::LiteralInt(i),
            span,
        }) => Ok(Spanned {
            span,
            value: ast::Expr::Literal(ast::LiteralData::Integral(i)),
        }),
        Some(t) => todo!("Unexpected token while parsing an expression: {:?}", t),
        None => Err(ParseError::ExpectedExpr {
            span: expr_span,
            actual: None,
        }),
    };
}

#[must_use]
fn extract_doc_comments<'a>(stream: &'a mut PTokenStream) -> Vec<Token<'a>> {
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
    values: Vec<Spanned<Value>>,
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
                .with_message("TODO")
                .with_code("E0005")
                .with_labels(vec![Label::primary(file_id, span).with_message("")]),
            ParseError::ExpectedExpr { span, actual } => Diagnostic::error()
                .with_message("Expected expression")
                .with_code("E0006")
                .with_labels(vec![actual
                    .map(|t| {
                        Label::primary(file_id.clone(), span.clone())
                            .with_message(format!("Expected expression, but found: {:?}", t))
                    })
                    .unwrap_or_else(|| {
                        Label::primary(file_id.clone(), span)
                            .with_message("Expected expression".to_string())
                    })]),
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
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_empty_source() {
        let source: &str = "   \n  ";
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedModuleDefinition {
            span: 0..6,
            actual: None,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_identifier() {
        let source: &str = "module";
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedModuleName {
            span: 0..6,
            actual: None,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_identifier() {
        let source: &str = "module _";
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedModuleName {
            span: 0..8,
            actual: Some(TokenKind::NilIdentifier),
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_missing_where() {
        let source: &str = "module Test";
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedWhereStatement {
            span: 0..11,
            actual: None,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_module_incorrect_where() {
        let source: &str = "module Test when";
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedWhereStatement {
            span: 12..16,
            actual: Some(TokenKind::When),
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_unexpected_remainder() {
        let expected = ParseError::ExpectedEOF {
            actual: vec![Token {
                kind: TokenKind::Identifier("Extra"),
                span: 18..23,
            }],
        };

        {
            let source: &str = "module Test where Extra";
            let actual = parse(source).expect_err("Expected ParseError");

            assert_eq!(expected, actual);
        }
        {
            // with comment
            let source: &str = "module Test where Extra -- stuff";
            let actual = parse(source).expect_err("Expected ParseError");

            assert_eq!(expected, actual);
        }
        {
            // with doc comment
            let source: &str = "module Test where Extra --! extra stuff";
            let actual = parse(source).expect_err("Expected ParseError");

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
        let actual = parse(source).expect("Successful parse");

        let expected = Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                values: vec![],
            },
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_lowercase() {
        let source: &str = test_source::EMPTY_MODULE_LOWERCASE;
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `test`".to_string(),
            span: 20..24,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_module_underscore() {
        let source: &str = test_source::EMPTY_MODULE_UNDERSCORE;
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::UnexpectedModuleName {
            message: "Module name must start with a capital letter. Found: `_Test`".to_string(),
            span: 20..25,
        };

        assert_eq!(expected, actual);
    }

    //     #[test]
    //     fn test_orphaned_identifier() {
    //         let source: &str = r#"
    //             module Test
    //             where
    //
    //             thing
    // "#;
    //         let actual = parse(source).expect_err("Expected ParseError");
    //
    //         let expected = ParseError::OrphanedIdentifier {
    //             span: 56..61,
    //             name: "thing".to_string(),
    //         };
    //
    //         assert_eq!(expected, actual);
    //     }

    #[test]
    fn test_partial_value_declaration_with_eq() {
        let source: &str = r#"
            module Test
            where

            thing =
"#;
        let actual = parse(source).expect_err("Expected ParseError");

        let expected = ParseError::ExpectedExpr {
            span: 56..63,
            actual: None,
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_int_value_declaration_no_type() {
        let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
        let actual = parse(source).expect("Successful parse");

        let expected = Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
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
        };

        assert_eq!(expected, actual);
    }
    //
    // #[test]
    // fn test_int_value_declaration_with_type() {
    //     let source: &str = test_source::INT_VALUE_DECLARATION_WITH_TYPE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_float_value_declaration_no_type() {
    //     let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_NO_TYPE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_float_value_declaration_with_type() {
    //     let source: &str = test_source::FLOAT_VALUE_DECLARATION_WITH_TYPE;
    //
    //     assert_no_errors(source)
    // }
    //
    // #[test]
    // fn test_single_arg_function_declaration_with_type() {
    //     let source: &str = test_source::SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE;
    //
    //     assert_no_errors(source)
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
