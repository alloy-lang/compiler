#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Path {
    ThisModule {
        name: Name,
        subname: Option<Name>,
        scope: ScopeIdx,
    },
    OtherModule(Fqn),
    Unknown(NonEmpty<Name>),
}

impl Path {
    pub(crate) fn this_module(
        rest: impl IntoIterator<Item = impl Into<Name>>,
        first: impl Into<Name>,
        scope: ScopeIdx,
    ) -> Self {
        Self::ThisModule {
            name: first.into(),
            subname: rest.into_iter().next().map(Into::into),
            scope,
        }
    }
}
