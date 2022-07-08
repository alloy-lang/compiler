use core::convert;
use std::iter;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use improved_slice_patterns::match_vec;
use non_empty_vec::NonEmpty;

// use crate::docs::extract_doc_comments;
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
mod type_variables;

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
            let (imports, type_annotations, values, type_definitions, traits) = module::parse_module_contents(remainder)?;

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

type ParseResult<'a, T> = Result<(Spanned<T>, Vec<Token<'a>>), ParseError<'a>>;

//
// Types that represent the Parse Tree
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
    fn span_end(&self) -> usize {
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
pub enum TypeConstraint {
    Kind { args: usize },
    Trait(String),
}

impl TypeConstraint {
    #[must_use]
    fn new_kind(args: usize) -> Self {
        TypeConstraint::Kind { args }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeVariable {
    id: Spanned<String>,
    constraints: Vec<Spanned<TypeConstraint>>,
}

impl TypeVariable {
    #[must_use]
    fn new_free<S>(id: S, span: Span) -> Self
    where
        S: Into<String>,
    {
        TypeVariable {
            id: Spanned {
                span,
                value: id.into(),
            },
            constraints: vec![],
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeAnnotation {
    name: Spanned<String>,
    t: Spanned<ast::Type>,
    type_variables: Vec<Spanned<TypeVariable>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct TypeDefinition {
    name: Spanned<String>,
    binds: Vec<Spanned<ast::Type>>,
    types: Spanned<NonEmpty<Spanned<NamedType>>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct NamedType {
    name: Spanned<String>,
    t: Option<Spanned<ast::Type>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Trait {
    name: Spanned<String>,
    self_constraints: Vec<Spanned<TypeConstraint>>,
    type_variables: Vec<Spanned<String>>,
    type_annotations: Vec<Spanned<TypeAnnotation>>,
    // values: Vec<Spanned<Value>>,
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
    ExpectedBoundTypeConstraint {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedBoundTypeConstraints {
        span: Span,
        actual: Vec<Token<'a>>,
    },
    ExpectedBoundTypeComma {
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
    ExpectedTraitEndKeyWord {
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
            ParseError::ExpectedBoundTypeConstraint { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span).with_message(format!(
                            "Expected bounded type constraint, but found: {:?}",
                            t.kind
                        ))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span).with_message("Expected return type"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message("We encountered an error while parsing a bound type. We expected the bound type to be constrained by a Type, ex: `List<String>` instead of `List<\"thing\">`")
                    .with_code("E0008")
                    .with_labels(labels)
            }
            ParseError::ExpectedBoundTypeConstraints { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span).with_message(format!(
                            "Expected bounded type to have constraints, but found: {:?}",
                            t.kind
                        ))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span).with_message("Expected return type"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message("We encountered an error while parsing a bound type. We expected the bound type to have some constraints, ex: `List<String>` instead of `List<>`")
                    .with_code("E0008")
                    .with_labels(labels)
            }
            ParseError::ExpectedBoundTypeComma { span, actual } => {
                let labels = actual
                    .into_iter()
                    .map(|t| {
                        Label::secondary(file_id.clone(), t.span)
                            .with_message(format!("Expected a comma, but found: {:?}", t.kind))
                    })
                    .chain(iter::once(
                        Label::primary(file_id.clone(), span)
                            .with_message("Expected a comma between bound type constraints"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message(
                        "We encountered an error while parsing a bound type. We expected the bound type to have a comma between constraints, ex: `Tuple<Int, String>` instead of `Tuple<Int String>`",
                    )
                    .with_code("E0013")
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
                        "Expected 'else' following 'if then'. Ex: `if (num > 1) then 1 else 0` instead of `if (num > 1) then 1 0`.",
                    )
                    .with_code("E0013")
                    .with_labels(labels)
            }
            ParseError::ExpectedTraitEndKeyWord { span, actual: _ } => {
                let labels = vec![Label::primary(file_id, span)
                    .with_message("Expected 'trait' definition to end with 'end'.")];

                Diagnostic::error()
                    .with_message(
                        "Expected 'trait' definition to end with 'end'. Ex: `trait Example where ... end` instead of `trait Example where ...`.",
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
    use super::parse;
    use std::fs;

    #[ignore]
    #[test]
    fn test_std_lib() {
        let std_lib = fs::read_dir("../../std")
            .expect("Something went wrong reading the std lib dir")
            .map(|res| {
                res.expect("Something went wrong reading the directory entry")
                    .path()
            })
            .collect::<Vec<_>>();

        for path in std_lib {
            let file_name = path.to_str().expect("Expected filename");

            let source = fs::read_to_string(file_name).expect(&format!(
                "Something went wrong reading the file '{:?}'",
                file_name
            ));
            let actual = parse(&source);

            assert!(
                actual.is_ok(),
                "file '{}' contained: {:?}",
                file_name,
                actual
            );
        }
    }
}
