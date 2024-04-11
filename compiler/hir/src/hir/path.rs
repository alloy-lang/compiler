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
        last: impl Into<Name>,
    ) -> Self {
        Self::ThisModule(NonEmpty::from((
            rest.into_iter().map(Into::into).collect(),
            last.into(),
        )))
    }
}
