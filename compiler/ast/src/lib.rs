use non_empty_vec::NonEmpty;
use ordered_float::NotNan;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Module {
    name: String,
    imports: Vec<Import>,
    typedefs: Vec<Typedef>,
    traits: Vec<Trait>,
    behavior: Vec<Behavior>,
    values: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Import {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Typedef {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Trait {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Behavior {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub struct Value {}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Type {
    Identifier(String),
    Variable(String),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Expr {
    // Identifier(String),
    // Apply(Box<Expr>, Box<Expr>),
    // OpApply(Box<Expr>, String, Box<Expr>),
    Literal(LiteralData),
    // Lambda(Pattern, Box<Expr>),
    // Case(Box<Expr>, NonEmpty<Alternative>),
    // IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum Pattern {
    Literal(LiteralData),
    Identifier(String),
    Tuple(NonEmpty<Pattern>),
    // Constructor(String, NonEmpty<Pattern>),
    // WildCard,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum LiteralData {
    Integral(i64),
    Fractional(NotNan<f64>),
    // String(String),
    // Char(char),
}

impl LiteralData {
    pub fn fractional(val: f64) -> Self {
        LiteralData::Fractional(NotNan::new(val).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};
}
