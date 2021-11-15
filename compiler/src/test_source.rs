pub(crate) const EMPTY_MODULE: &str = r#"
            module Test
            where



"#;

pub(crate) const VALUE_DECLARATION_NO_TYPE: &str = r#"
            module Test
            where

            thing = 0
"#;

pub(crate) const VALUE_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            thing : Int
            thing = 0
"#;

pub(crate) const VALUE_DECLARATION_WITH_CONFLICTING_TYPE_ANNOTATIONS: &str = r#"
            module Test
            where

            thing : String
            thing : Int
            thing = 0
"#;

pub(crate) const TYPE_ANNOTATION_WITH_NO_DEFINITION: &str = r#"
                module Test
                where

                thing : String
"#;

pub(crate) const SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"


            module Test
            where

            increment_positive : Int -> Int
            increment_positive = |0| => 0
            increment_positive = |x| => x + 1
"#;

pub(crate) const MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, "")| => 0
            increment_by_length = |(x, y)| => x + String::length(y)
"#;

pub(crate) const CURRIED_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            increment_by_length : (Int -> Int) -> Int -> Int
            increment_by_length = |f| => |value| => f(value)
"#;

pub(crate) const SIMPLE_IF_THEN_ELSE: &str = r#"
            module Test
            where

            increment_positive = |num| =>
              if Number::is_positive?(num)
              then num + 1
              else num
            decrement_negative = |num| =>
              if Number::is_negative?(num)
              then num - 1
              else num
"#;

pub(crate) const NESTED_IF_THEN_ELSE: &str = r#"
            module Test
            where

            increment_or_decrement = |num| =>
              if Number::is_positive?(num)
              then num + 1
              else
                if Number::is_negative?(num)
                then num - 1
                else num
"#;

pub(crate) const MULTI_PROPERTY_UNION_TYPE_1: &str = r#"
            module Test
            where

            data Either<L, R> =
              | (:Right, R)
              | (:Left, L)
"#;
pub(crate) const MULTI_PROPERTY_UNION_TYPE_2: &str = r#"
            module Test
            where

            data Either<L, R> =
              (:Right, R)
              | (:Left, L)
"#;
pub(crate) const MULTI_PROPERTY_UNION_TYPE_3: &str = r#"
            module Test
            where

            data Either<L, R> = (:Right, R) | (:Left, L)
"#;
