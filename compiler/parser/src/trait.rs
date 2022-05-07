use alloy_lexer::{Token, TokenKind, T};
use core::convert;

use improved_slice_patterns::match_vec;

use super::r#type;
use super::{ParseError, ParseResult, Span, Spanned, TypeConstraint};

pub type TraitContents = (Vec<Spanned<String>>, Vec<Spanned<TypeConstraint>>);

pub fn parse<'a>(
    trait_keyword_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(TraitContents, Span, Vec<Token<'a>>), ParseError<'a>> {
    let mut type_variables = vec![];
    let mut self_constraints = vec![];

    let mut remainder = input.collect::<Vec<_>>();

    while !remainder.is_empty() {
        log::debug!("*parse_module_contents* remainder: {:?}", remainder);

        remainder = match_vec!(remainder;
            [
                Token { kind: T![typevar],                    span: typevar_token_span },
                Token { kind: TokenKind::LowerIdentifier(id), span: id_span },
                remainder @ ..
            ] => {
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

                loop {
                    remainder = match_vec!(remainder.clone();
                        [
                            Token { kind: TokenKind::KindMarker(args), span: kind_marker_span },
                            remainder @ ..
                        ] => {
                            self_constraints.push(Spanned {
                                span: kind_marker_span,
                                value: TypeConstraint::Kind { args },
                            });

                            remainder.collect()
                        },

                        [
                            Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
                            remainder @ ..
                        ] => {
                            self_constraints.push(Spanned {
                                span: id_span,
                                value: TypeConstraint::Trait(id.to_string()),
                            });

                            remainder.collect()
                        },

                        [
                            Token { kind: TokenKind::Plus, span: plus_span },
                            remainder @ ..
                        ] => {
                            remainder.collect()
                        },

                        [
                            remainder @ ..
                        ] => {
                            break;
                        }
                    )
                    .map_err(|remaining| ParseError::ExpectedLambdaArgsComma {
                        span: eq_span.clone(),
                        actual: remaining,
                    })?;
                }

                Ok(remainder)
            },
            //
            // [
            //     Token { kind: T![|],  span: end_pipe_span },
            //     remainder @ ..
            // ] => {
            //     let span = start_pipe_span.start..end_pipe_span.end;
            //     Err(ParseError::ExpectedRightArrow {
            //         span,
            //         actual: pattern_remainder.clone(),
            //     })
            // },
            //
            // [remainder @ ..] => {
            //     let span = expr_span.clone();
            //     let span = span.start..pattern_remainder.get(0).map_or(span, Token::span).end;
            //     Err(ParseError::ExpectedPipe {
            //         span,
            //         actual: pattern_remainder.clone(),
            //     })
            // },

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

    Ok((
        (type_variables, self_constraints),
        trait_keyword_span.clone(),
        remainder,
    ))
}

#[cfg(test)]
mod trait_parser_tests {
    use non_empty_vec::NonEmpty;
    use ordered_float::NotNan;
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned, Trait, TypeAnnotation, TypeConstraint, Value};

    #[test]
    fn test_empty_trait_with_comment() {
        let source = r#"
            module Test
            where

            --! This is a comment
            trait TestTrait where
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
                    span: 90..111,
                    value: Trait {
                        name: Spanned {
                            span: 96..105,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![],
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
                    span: 56..77,
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
                    span: 56..77,
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
                    },
                }],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_trait_with_value_with_typevar() {
        let source = r#"
            module Test
            where

            trait TestTrait where
                wrap : t1 -> self<t1> where
                  typevar t1
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
                    span: 13..42,
                    value: Trait {
                        name: Spanned {
                            span: 20..24,
                            value: "TestTrait".to_string(),
                        },
                        self_constraints: vec![],
                        type_variables: vec![],
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
    //             typevar self = #Type<_> + Functor
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
