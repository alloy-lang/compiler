#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    ThisModule(NonEmpty<Name>),
    OtherModule(Fqn),
    Unknown(NonEmpty<Name>),
}

impl Path {
    pub(crate) fn this_module(
        rest: impl IntoIterator<Item = impl Into<Name>>,
        first: impl Into<Name>,
    ) -> Self {
        Self::ThisModule(NonEmpty::from((
            first.into(),
            rest.into_iter().map(Into::into).collect(),
        )))
    }
}
