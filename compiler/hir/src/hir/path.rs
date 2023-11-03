#[allow(clippy::wildcard_imports)]
use super::*;

use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    ThisModule(Name),
    OtherModule(Fqn),
}

impl From<NonEmpty<String>> for Path {
    fn from(value: NonEmpty<String>) -> Self {
        let (target, rest) = value.split_last();

        if rest.is_empty() {
            Self::ThisModule(Name::new(target))
        } else {
            Self::OtherModule(Fqn::new(rest, target))
        }
    }
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Self::ThisModule(Name::new(value))
    }
}

#[derive(Debug, PartialEq)]
pub struct EmptyError;

impl TryFrom<Vec<String>> for Path {
    type Error = EmptyError;
    fn try_from(value: Vec<String>) -> Result<Self, Self::Error> {
        let Ok(value) = NonEmpty::try_from(value) else {
            return Err(EmptyError);
        };

        Ok(Path::from(value))
    }
}
