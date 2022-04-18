#[cfg(test)]
mod imports_parser_tests {
    use non_empty_vec::NonEmpty;
    use pretty_assertions::assert_eq;

    use crate::{parse, Import, Module, ParseError, Spanned};

    #[test]
    fn test_imports() {
        let source = r#"
            module Test
            where

            import std::pretty::long::import::path
            import std::bool::Bool
            import std::bool::not
            import std::function::(<|)
            import std
        "#;
        let actual = parse(source);

        let expected = Ok(Spanned {
            span: 13..42,
            value: Module {
                name: Spanned {
                    span: 20..24,
                    value: "Test".to_string(),
                },
                imports: vec![
                    spanned_import(56, 94, vec!["std", "pretty", "long", "import", "path"]),
                    spanned_import(107, 129, vec!["std", "bool", "Bool"]),
                    spanned_import(142, 163, vec!["std", "bool", "not"]),
                    spanned_import(176, 202, vec!["std", "function", "(<|)"]),
                    spanned_import(215, 225, vec!["std"]),
                ],
                type_annotations: vec![],
                values: vec![],
                type_definitions: vec![],
            },
        });

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_incomplete_import() {
        let source = r#"
            module Test
            where

            import std::pretty::long::unfinished::
        "#;
        let actual = parse(source);

        let expected = Err(ParseError::InvalidImport {
            span: 63..94,
            message: "Import must have a path separated by '::'. Found: `std::pretty::long::unfinished::`".to_string(),
        });

        assert_eq!(expected, actual);
    }

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

    fn spanned_import(start: usize, end: usize, segments: Vec<&str>) -> Spanned<Import> {
        Spanned {
            span: start..end,
            value: Import {
                import: Spanned {
                    span: (start + 7)..end,
                    value: unsafe {
                        NonEmpty::new_unchecked(
                            segments.into_iter().map(ToString::to_string).collect(),
                        )
                    },
                },
            },
        }
    }
}
