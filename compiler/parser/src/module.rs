use core::convert;

use improved_slice_patterns::match_vec;
use non_empty_vec::NonEmpty;

use super::{Import, ParseError, Spanned, Trait, TypeAnnotation, TypeDefinition, Value};
use alloy_lexer::{Token, TokenKind, T};

use crate::expr;
use crate::r#trait;
use crate::r#type;
use crate::type_definition;
use crate::type_variables;

type ModuleContents = (
    Vec<Spanned<Import>>,
    Vec<Spanned<TypeAnnotation>>,
    Vec<Spanned<Value>>,
    Vec<Spanned<TypeDefinition>>,
    Vec<Spanned<Trait>>,
);

pub fn parse_module_contents<'a>(
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
                    let (type_variables, remainder) = type_variables::parse(remainder)?;

                    let type_annotation = Spanned {
                        span: type_span.start..t.span_end(),
                        value: TypeAnnotation {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            t,
                            type_variables,
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
                        span: expr_span.start..expr.span_end(),
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
                    Token { kind: T![typedef],                    span: typedef_span },
                    Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                    Token { kind: T![=],                          span: eq_span },
                    remainder @ ..
                ] => {
                    let type_span = typedef_span.start..eq_span.end;

                    let (binds, types, remainder) = type_definition::parse(&type_span, remainder)?;

                    let type_definition = Spanned {
                        span: typedef_span.start..types.span_end(),
                        value: TypeDefinition {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            binds,
                            types,
                        }
                    };

                    type_definitions.push(type_definition);

                    Ok(remainder)
                },

                [
                    Token { kind: T![typedef],                    span: typedef_span },
                    Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                    Token { kind: T![<],                          span: gt_span },
                    remainder @ ..
                ] => {
                    let type_span = typedef_span.start..gt_span.end;
                    let (furthest_span, binds, remainder) = r#type::parse_binds(type_span, remainder.peekable())?;

                    let type_span = typedef_span.start..furthest_span.end;
                    let (binds, types, remainder) = type_definition::parse(&type_span, remainder.into_iter())?;

                    let type_definition = Spanned {
                        span: typedef_span.start..types.span_end(),
                        value: TypeDefinition {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            binds,
                            types,
                        }
                    };

                    type_definitions.push(type_definition);

                    Ok(remainder)
                },

                [
                    Token { kind: T![typedef],                    span: typedef_span },
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

                    let ((self_constraints, type_variables, type_annotations), trait_span, remainder) = r#trait::parse(&trait_span, remainder)?;

                    let traitt = Spanned {
                        span: trait_span,
                        value: Trait {
                            name: Spanned {
                                span: id_span,
                                value: id.to_string(),
                            },
                            self_constraints,
                            type_variables,
                            type_annotations,
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

#[cfg(test)]
mod module_parser_tests {
    use pretty_assertions::assert_eq;

    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned};

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
                traits: vec![],
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
}
