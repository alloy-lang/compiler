#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, PartialEq)]
pub enum TypeVariable {
    Unbound,
    Bound(Type),
}
