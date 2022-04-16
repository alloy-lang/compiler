use non_empty_vec::NonEmpty;

use alloy_lexer::{Token, TokenKind};

use crate::Trait;

use super::r#type;
use super::{ParseError, ParseResult, Span, Spanned};

fn parse<'a>(
    trait_keyword_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> ParseResult<'a, Trait> {

}


#[cfg(test)]
mod trait_parser_tests {
    use non_empty_vec::NonEmpty;
    use ordered_float::NotNan;
    use pretty_assertions::assert_eq;

    use alloy_ast as ast;
    use alloy_lexer::{Token, TokenKind};

    use crate::{parse, Module, ParseError, Spanned, TypeAnnotation, Value};

    #[test]
    fn test_empty_trait() {
        let source = r#"
            module Applicative
            where

            import std::functor::Functor

            --! `Functor` with application.
            --!
            --! The following laws should hold:
            --!
            --! * `apply(wrap(id), v) == v`
            --! * `apply(apply(apply(wrap(<<), u), v), w) == apply(u, apply(v, w))`
            --! * `apply(wrap(f), wrap(x)) == wrap(f(x))`
            --! * `apply(u, wrap(y)) == apply(wrap(|g| -> g(x)), u)`
            trait Applicative where
                typevar self = #Type<_> + Functor

                --! Similar to `Functor::map`
                --! but this time the supplied function `t1 -> t2` is embedded in `self`
                apply : self<(t1 -> t2)> -> self<t1> -> self<t2> where
                  typevar t1
                  typevar t2

                --! Wrap the supplied value into `self<_>`
                --!
                --! # Examples
                --!
                --! * `Applicative<Option<_>>::wrap(1) == Option::Some(1)`
                --! * `Applicative<Either<_, _>>::wrap(1) == Either::Right(1)`
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
            },
        });

        assert_eq!(expected, actual);
    }
}