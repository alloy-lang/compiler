pub const EMPTY_MODULE: &str = r#"
            module Test
            where



"#;

pub const EMPTY_MODULE_LOWERCASE: &str = r#"
            module test
            where



"#;

pub const EMPTY_MODULE_UNDERSCORE: &str = r#"
            module _Test
            where



"#;

pub const INT_VALUE_DECLARATION_WITH_NO_TYPE: &str = r#"
            module Test
            where

            thing = 0
"#;

pub const INT_VALUE_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            thing : Int
            thing = 0
"#;

pub const FLOAT_VALUE_DECLARATION_WITH_NO_TYPE: &str = r#"
            module Test
            where

            thing = 0.1
"#;

pub const FLOAT_VALUE_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            thing : Float
            thing = 0.1
"#;

pub const INT_VALUE_DECLARATION_WITH_CONFLICTING_TYPE_ANNOTATIONS: &str = r#"
            module Test
            where

            thing : String
            thing : Int
            thing = 0
"#;

// pub const TYPE_ANNOTATION_WITH_NO_DEFINITION: &str = r#"
//                 module Test
//                 where
//
//                 thing : String
// "#;

pub const VALUE_WITH_CONFLICTING_TYPE_ANNOTATIONS: &str = r#"
                module Test
                where

                thing : String
                thing : Int
                thing = 0.0
"#;

pub const SINGLE_ARG_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"


            module Test
            where

            increment_positive : Int -> Int
            increment_positive = |0| => 0
            increment_positive = |x| => x + 1
"#;
pub const SINGLE_ARG_FUNCTION_DECLARATION_WITH_NO_TYPE: &str = r#"


            module Test
            where

            increment_positive = |0| => 0
            increment_positive = |x| => x + 1
"#;

pub const MULTI_ARG_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            increment_by_length : (Int, String) -> Int
            increment_by_length = |(0, "")| => 0
            increment_by_length = |(x, y)| => x + String::length(y)
"#;

pub const MULTI_ARG_FUNCTION_DECLARATION_WITH_NO_TYPE: &str = r#"
            module Test
            where

            increment_by_length = |(0, "")| => 0
            increment_by_length = |(x, y)| => x + String::length(y)
"#;

pub const CURRIED_FUNCTION_DECLARATION_WITH_TYPE: &str = r#"
            module Test
            where

            apply : (Int -> Int) -> Int -> Int
            apply = |f| => |value| => f(value)
"#;

pub const CURRIED_FUNCTION_DECLARATION_WITH_NO_TYPE: &str = r#"
            module Test
            where

            apply = |f| => |value| => f(value)
"#;

pub const SIMPLE_IF_THEN_ELSE: &str = r#"
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

pub const NESTED_IF_THEN_ELSE: &str = r#"
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

pub const CALL_FUNCTION_IN_MODULE_WITH_TYPE: &str = r#"
            module Test
            where

            max : (Int, Int) -> Int
            max = |(first, second)| =>
              if first > second
              then first
              else second

            sum_two_largest : (Int, Int, Int) -> Int
            sum_two_largest = |(num1, num2, num3)| =>
              if num1 == max((num1, num2))
              then num1 + max((num2, num3))
              else num2 + max((num1, num3))
"#;

pub const CALL_FUNCTION_IN_MODULE_WITH_ONE_TYPE: &str = r#"
            module Test
            where

            max : (Int, Int) -> Int
            max = |(first, second)| =>
              if first > second
              then first
              else second

            sum_two_largest = |(num1, num2, num3)| =>
              if num1 == max((num1, num2))
              then num1 + max((num2, num3))
              else num2 + max((num1, num3))
"#;

pub const CALL_FUNCTION_IN_MODULE_WITH_NO_TYPE: &str = r#"
            module Test
            where

            max = |(first, second)| =>
              if first > second
              then first
              else second

            sum_two_largest = |(num1, num2, num3)| =>
              if num1 == max((num1, num2))
              then num1 + max((num2, num3))
              else num2 + max((num1, num3))
"#;

pub const MULTI_PROPERTY_UNION_TYPE_1: &str = r#"
            module Test
            where

            data Either<L, R> =
              | (:Right, R)
              | (:Left, L)
"#;
pub const MULTI_PROPERTY_UNION_TYPE_2: &str = r#"
            module Test
            where

            data Either<L, R> =
              (:Right, R)
              | (:Left, L)
"#;
pub const MULTI_PROPERTY_UNION_TYPE_3: &str = r#"
            module Test
            where

            data Either<L, R> = (:Right, R) | (:Left, L)
"#;

pub const CONFLICTING_TYPE_ALIAS_DEFINITIONS: &str = r#"
            module Test
            where

            data Bool<> = :False | :True

            data Bool<> = :True | :False
"#;
