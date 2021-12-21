use itertools::Itertools;
use std::iter::Peekable;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use alloy_lexer::{Token, TokenKind, TokenStream};

//
// parse functions
//

pub fn parse<'a>(source: &str) -> Result<Module, ParseError> {
    let mut stream = TokenStream::from_source(source).peekable();

    let _doc_comments = extract_doc_comments(&mut stream);

    while let Some(token) = stream.next() {
        match token.kind {
            TokenKind::Module => {
                return parse_module(token.span, &mut stream);
            }
            TokenKind::EOL => {}
            _ => todo!("Unhandled token kind"),
        }
    }

    Err(ParseError::ExpectedModuleDefinition {
        span: 0..source.len(),
        actual: None,
    })
}

fn parse_module<'a>(
    module_token_span: Span,
    stream: &mut Peekable<TokenStream<'a>>,
) -> Result<Module, ParseError<'a>> {
    let _doc_comments = extract_doc_comments(stream);

    if let Some(module_id_token) = stream.next() {
        if let TokenKind::Identifier(id) = module_id_token.kind {
            let _doc_comments = extract_doc_comments(stream);

            if let Some(where_token) = stream.next() {
                if let TokenKind::Where = where_token.kind {
                    if let Some(first) = id.chars().next() {
                        if first.is_ascii_uppercase() {
                            Ok(Module {
                                name: id.to_string(),
                            })
                        } else {
                            Err(ParseError::UnexpectedModuleName {
                                message: format!(
                                    "Module name must start with a capital letter. Found: `{}`",
                                    id
                                ),
                                span: module_id_token.span,
                            })
                        }
                    } else {
                        Err(ParseError::UnexpectedModuleName {
                            message: "Module name must not be empty".to_string(),
                            span: module_id_token.span,
                        })
                    }
                } else {
                    Err(ParseError::ExpectedWhereStatement {
                        span: where_token.span,
                        actual: Some(where_token.kind),
                    })
                }
            } else {
                Err(ParseError::ExpectedWhereStatement {
                    span: module_token_span.start..module_id_token.span.end,
                    actual: None,
                })
            }
        } else {
            Err(ParseError::ExpectedModuleName {
                span: module_token_span.start..module_id_token.span.end,
                actual: Some(module_id_token.kind),
            })
        }
    } else {
        Err(ParseError::ExpectedModuleName {
            span: module_token_span,
            actual: None,
        })
    }
}

#[must_use]
fn extract_doc_comments<'a>(stream: &'a mut Peekable<TokenStream>) -> Vec<Token<'a>> {
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

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Module {
    name: String,
}

type Span = Range<usize>;

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
}

impl<'a> ParseError<'a> {
    pub fn to_diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId> {
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
                .with_message("Modules must have a `where` statement. Ex: `module HelloWorld where`.")
                .with_code("E0003")
                .with_labels(vec![
                    Label::primary(file_id, span).with_message("expected a where statement")
                ]),
            ParseError::UnexpectedModuleName { message, span } => Diagnostic::error()
                .with_message(
                    "Module names must start with a capital letter. Ex: `module HelloWorld where`.",
                )
                .with_code("E0003")
                .with_labels(vec![Label::primary(file_id, span).with_message(message)]),
        }
    }
}

#[cfg(test)]
mod tests {
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
    fn test_empty_module() {
        let source: &str = test_source::EMPTY_MODULE;
        let actual = parse(source).expect("Successful parse");

        let expected = Module {
            name: "Test".to_string(),
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

    // #[test]
    // fn test_int_value_declaration_no_type() {
    //     let source: &str = test_source::INT_VALUE_DECLARATION_WITH_NO_TYPE;
    //
    //     assert_no_errors(source)
    // }
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
