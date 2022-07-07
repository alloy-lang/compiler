use alloy_lexer::{Token, TokenKind, T};
use core::convert;
use std::iter;

use improved_slice_patterns::match_vec;

use super::r#type;
use super::{ParseError, Span, Spanned, TypeAnnotation, TypeConstraint};

use crate::type_variables;

pub type TraitContents = (
    Vec<Spanned<TypeConstraint>>,
    Vec<Spanned<String>>,
    Vec<Spanned<TypeAnnotation>>,
);

pub fn parse<'a>(
    starting_trait_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(TraitContents, Span, Vec<Token<'a>>), ParseError<'a>> {
    let mut self_constraints = vec![];
    let mut type_variables = vec![];
    let mut type_annotations = vec![];

    let mut furthest_character_position = starting_trait_span.end;

    let mut remainder = input.collect::<Vec<_>>();
    let mut cont = !remainder.is_empty();
    while cont {
        log::debug!("*parse_module_contents* remainder: {:?}", remainder);

        remainder = match_vec!(remainder;
            [
                Token { kind: T![typevar],                    span: typevar_token_span },
                Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                remainder @ ..
            ] => {
                furthest_character_position = id_span.end;
                type_variables.push(Spanned {
                    span: id_span,
                    value: id.to_string(),
                });

                Ok(remainder.collect())
            },

            [
                Token { kind: T![self], span: self_token_span },
                Token { kind: T![=],    span: eq_span },
                remainder @ ..
            ] => {
                let mut remainder = remainder.collect::<Vec<_>>();

                let mut cont = true;
                while cont {
                    remainder = match_vec!(remainder.clone();
                        [
                            Token { kind: TokenKind::KindMarker(args), span: kind_marker_span },
                            remainder @ ..
                        ] => {
                            furthest_character_position = kind_marker_span.end;
                            self_constraints.push(Spanned {
                                span: kind_marker_span,
                                value: TypeConstraint::new_kind(args),
                            });

                            remainder.collect()
                        },

                        [
                            Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                            remainder @ ..
                        ] => {
                            furthest_character_position = id_span.end;
                            self_constraints.push(Spanned {
                                span: id_span,
                                value: TypeConstraint::Trait(id.to_string()),
                            });

                            remainder.collect()
                        },

                        [
                            Token { kind: T![+], span: plus_span },
                            remainder @ ..
                        ] => {
                            furthest_character_position = plus_span.end;
                            remainder.collect()
                        },

                        [
                            remainder @ ..
                        ] => {
                            cont = false;
                            remainder.collect()
                        }
                    )
                    .map_err(|remaining| todo!("ParseError: did not match anything...wat? 1234"))?;
                    // .map_err(|remaining| ParseError::ExpectedLambdaArgsComma {
                    //     span: eq_span.clone(),
                    //     actual: remaining,
                    // })?;
                }

                Ok(remainder)
            },

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

                furthest_character_position = type_annotation.span_end();
                type_annotations.push(type_annotation);

                Ok(remainder)
            },

            [
                Token { kind: T![end],  span: end_marker_span },
                remainder @ ..
            ] => {
                cont = false;
                furthest_character_position = end_marker_span.end;

                Ok(remainder.collect())
            },

            [
                remainder @ ..,
                Token { kind: TokenKind::EOF, span: eof_span }
            ] => {
                let next_span = remainder.clone().next().map_or_else(|| eof_span.clone(), |s| s.span);

                Err(ParseError::ExpectedTraitEndKeyWord {
                    span: furthest_character_position..next_span.start,
                    actual: remainder
                        .chain(iter::once(Token { kind: TokenKind::EOF, span: eof_span }))
                        .collect(),
                })
            }
        )
        .map_err(|remaining| ParseError::ExpectedEOF {
            input: vec![],
            remaining,
        })
        .and_then(convert::identity)?;

        cont = cont && !remainder.is_empty();
    }

    Ok((
        (self_constraints, type_variables, type_annotations),
        starting_trait_span.start..furthest_character_position,
        remainder,
    ))
}

#[cfg(test)]
mod trait_parser_tests {
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{
        parse, Module, ParseError, Spanned, Trait, TypeAnnotation, TypeConstraint, TypeVariable,
    };

    #[test]
    fn test_single_trait_with_no_end_keyword_returns_error() {
        let source = r#"
            module Test
            where

            trait TestTrait1 where
                -- imagine all the things here
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedTraitEndKeyWord {
            span: 78..134,
            actual: vec![Token {
                kind: TokenKind::EOF,
                span: 134..134,
            }],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_multiple_traits_with_no_end_keyword_returns_error() {
        let source = r#"
            module Test
            where

            trait TestTrait1 where
                -- imagine all the things here

            trait TestTrait2 where
                -- there would be neat stuff here too!
            end
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::ExpectedTraitEndKeyWord {
            span: 78..139,
            actual: vec![
                Token {
                    kind: TokenKind::Trait,
                    span: 139..144,
                },
                Token {
                    kind: TokenKind::UpperIdentifier("TestTrait2"),
                    span: 145..155,
                },
                Token {
                    kind: TokenKind::Where,
                    span: 156..161,
                },
                Token {
                    kind: TokenKind::End,
                    span: 229..232,
                },
                Token {
                    kind: TokenKind::EOF,
                    span: 241..241,
                },
            ],
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_trait_with_comment() {
        let source = r#"
            module Test
            where

            --! This is a comment
            trait TestTrait where
            end
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
                type_definitions: vec![],
                traits: vec![Spanned {
                    span: 90..127,
                    value: Trait {
                        name: Spanned {
                            span: 96..105,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![],
                        type_annotations: vec![],
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_trait_with_free_typevar() {
        let source = r#"
            module Test
            where

            trait TestTrait where
                typevar t1
            end
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
                type_definitions: vec![],
                traits: vec![Spanned {
                    span: 56..120,
                    value: Trait {
                        name: Spanned {
                            span: 62..71,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![Spanned {
                            span: 102..104,
                            value: "t1".to_string(),
                        }],
                        type_annotations: vec![],
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_trait_with_self_typevar_constraints() {
        let source = r#"
            module Test
            where

            trait TestTrait where
                self = #Type<_> + Functor
            end
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
                type_definitions: vec![],
                traits: vec![Spanned {
                    span: 56..135,
                    value: Trait {
                        name: Spanned {
                            span: 62..71,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![
                            Spanned {
                                span: 101..109,
                                value: TypeConstraint::Kind { args: 1 },
                            },
                            Spanned {
                                span: 112..119,
                                value: TypeConstraint::Trait("Functor".to_string()),
                            },
                        ],
                        type_variables: vec![],
                        type_annotations: vec![],
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_trait_with_type_annotation_inside_and_outside() {
        let source = r#"
            module Test
            where

            trait TestTrait where
                inside_trait : Int
            end

            outside_trait : Int
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
                type_annotations: vec![Spanned {
                    span: 142..161,
                    value: TypeAnnotation {
                        name: Spanned {
                            span: 142..155,
                            value: "outside_trait".to_string(),
                        },
                        t: Spanned {
                            span: 158..161,
                            value: ast::Type::identifier("Int"),
                        },
                        type_variables: vec![],
                    },
                }],
                values: vec![],
                type_definitions: vec![],
                traits: vec![Spanned {
                    span: 56..128,
                    value: Trait {
                        name: Spanned {
                            span: 62..71,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![],
                        type_annotations: vec![Spanned {
                            span: 94..112,
                            value: TypeAnnotation {
                                name: Spanned {
                                    span: 94..106,
                                    value: "inside_trait".to_string(),
                                },
                                t: Spanned {
                                    span: 109..112,
                                    value: ast::Type::identifier("Int"),
                                },
                                type_variables: vec![],
                            },
                        }],
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_trait_with_type_annotation_with_typevar() {
        let source = r#"
            module Test
            where

            trait TestTrait where
                apply : self<(t1 -> t2)> -> self<t1> -> self<t2> where
                    typevar t1
                    typevar t2
            end
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
                type_definitions: vec![],
                traits: vec![Spanned {
                    span: 56..226,
                    value: Trait {
                        name: Spanned {
                            span: 62..71,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![],
                        type_annotations: vec![Spanned {
                            span: 94..142,
                            value: TypeAnnotation {
                                name: Spanned {
                                    span: 94..99,
                                    value: "apply".to_string(),
                                },
                                t: Spanned {
                                    span: 102..142,
                                    value: ast::Type::lambda(
                                        ast::Type::bound(
                                            ast::Type::SelfRef,
                                            vec![ast::Type::lambda(
                                                ast::Type::variable("t1"),
                                                ast::Type::variable("t2"),
                                            )],
                                        ),
                                        ast::Type::lambda(
                                            ast::Type::bound(
                                                ast::Type::SelfRef,
                                                vec![ast::Type::variable("t1")],
                                            ),
                                            ast::Type::bound(
                                                ast::Type::SelfRef,
                                                vec![ast::Type::variable("t2")],
                                            ),
                                        ),
                                    ),
                                },
                                type_variables: vec![
                                    Spanned {
                                        span: 169..179,
                                        value: TypeVariable::new_free("t1", 177..179),
                                    },
                                    Spanned {
                                        span: 200..210,
                                        value: TypeVariable::new_free("t2", 208..210),
                                    },
                                ],
                            },
                        }],
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    // #[test]
    // fn test_trait_with_self_typevar_constraints() {
    //     let source = r#"
    //         module Test
    //         where
    //
    //         --! `Functor` with application.
    //         --!
    //         --! The following laws should hold:
    //         --!
    //         --! * `apply(wrap(id), v) == v`
    //         --! * `apply(apply(apply(wrap(<<), u), v), w) == apply(u, apply(v, w))`
    //         --! * `apply(wrap(f), wrap(x)) == wrap(f(x))`
    //         --! * `apply(u, wrap(y)) == apply(wrap(|g| -> g(x)), u)`
    //         trait Applicative where
    //             self = #Type<_> + Functor
    //
    //             --! Similar to `Functor::map`
    //             --! but this time the supplied function `t1 -> t2` is embedded in `self`
    //             apply : self<(t1 -> t2)> -> self<t1> -> self<t2> where
    //               typevar t1
    //               typevar t2
    //
    //             --! Wrap the supplied value into `self<_>`
    //             --!
    //             --! # Examples
    //             --!
    //             --! * `Applicative<Option<_>>::wrap(1) == Option::Some(1)`
    //             --! * `Applicative<Either<_, _>>::wrap(1) == Either::Right(1)`
    //             wrap : t1 -> self<t1> where
    //               typevar t1
    //     "#;
    //     let actual = parse(source);
    //
    //     let expected = Ok(Spanned {
    //         span: 13..42,
    //         value: Module {
    //             name: Spanned {
    //                 span: 20..24,
    //                 value: "Test".to_string(),
    //             },
    //             imports: vec![],
    //             type_annotations: vec![],
    //             values: vec![],
    //             type_definitions: vec![],
    //             traits: vec![Spanned {
    //                 span: 13..42,
    //                 value: Trait {
    //                     name: Spanned {
    //                         span: 20..24,
    //                         value: "Applicative".to_string(),
    //                     },
    //                     type_variables: vec![],
    //                 },
    //             }],
    //         },
    //     });
    //
    //     assert_eq!(expected, actual);
    // }
}
