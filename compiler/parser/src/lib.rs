use core::convert;
use std::iter;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use improved_slice_patterns::match_vec;
use non_empty_vec::NonEmpty;

use crate::docs::extract_doc_comments;
use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind, TokenStream, T};

mod docs;
mod expr;
mod imports;
mod module;
mod parens;
mod pattern;
mod r#trait;
mod r#type;
mod type_definition;

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
            let (imports, type_annotations, values, type_definitions, traits) = parse_module_contents(remainder)?;

            Ok(Spanned::from(
                module_token_span,
                where_token_span,
                Module {
                    name: Spanned::from_span(id_token_span, id.to_string()),
                    imports,
                    type_annotations,
                    values,
                    type_definitions,
                    traits,
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
    Vec<Spanned<Trait>>,
);
type ParseResult<'a, T> = Result<(Spanned<T>, Vec<Token<'a>>), ParseError<'a>>;

fn parse_module_contents<'a>(
    input: impl Iterator<Item = Token<'a>>,
) -> Result<ModuleContents, ParseError<'a>> {
    let mut imports = vec![];
    let mut type_annotations = vec![];
    let mut values = vec![];
    let mut type_definitions = vec![];
    let mut traits = vec![];

    // let _doc_comments = extract_doc_comments(&mut input);

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
                    let type_span = type_def_span.start..eq_span.end;

                    let (types, remainder) = type_definition::parse(&type_span, remainder)?;

                    let type_definition = Spanned {
                        span: type_def_span.start..types.span.end,
                        value: TypeDefinition {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            types,
                        }
                    };

                    type_definitions.push(type_definition);

                    Ok(remainder)
                },

                [
                    Token { kind: T![typedef],                    span: type_def_span },
                    Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                    remainder @ ..
                ] => {
                    Err(ParseError::ExpectedEq {
                        span: id_span,
                        actual: remainder.collect(),
                    })
                },

                [
                    Token { kind: T![trait],                      span: trait_keyword_span },
                    Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                    Token { kind: T![where],                      span: where_span },
                    remainder @ ..
                ] => {
                    let trait_span = trait_keyword_span.start..where_span.end;

                    let ((type_variables, self_constraints), trait_end_span, remainder) = r#trait::parse(&trait_span, remainder)?;

                    let traitt = Spanned {
                        span: trait_span.start..trait_end_span.end,
                        value: Trait {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            self_constraints,
                            type_variables,
                        },
                    };

                    traits.push(traitt);

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

    Ok((imports, type_annotations, values, type_definitions, traits))
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
    traits: Vec<Spanned<Trait>>,
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
    types: Spanned<NonEmpty<Spanned<NamedType>>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct NamedType {
    name: Spanned<String>,
    t: Option<Spanned<ast::Type>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TypeConstraint {
    Kind { args: usize },
    Trait(String),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Trait {
    name: Spanned<String>,
    self_constraints: Vec<Spanned<TypeConstraint>>,
    type_variables: Vec<Spanned<String>>,
    // values: Spanned<NonEmpty<Spanned<Value>>>,
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
    ExpectedTypeName {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedEq {
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
            ParseError::ExpectedTypeName { span, actual } => Diagnostic::error()
                .with_message("Expected type name")
                .with_code("E0007")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected type name, but found: {:?}", t))
                        })
                        .collect(),
                ),
            ParseError::ExpectedEq { span, actual } => Diagnostic::error()
                .with_message("Expected equals sign")
                .with_code("E0007")
                .with_labels(
                    actual
                        .into_iter()
                        .map(|t| {
                            Label::primary(file_id.clone(), span.clone())
                                .with_message(format!("Expected equals sign, but found: {:?}", t))
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
    // use pretty_assertions::assert_eq;
    // use alloy_ast as ast;
    //
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
