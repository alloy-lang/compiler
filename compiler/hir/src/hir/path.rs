#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Path {
    ThisModule {
        path: NonEmpty<Name>,
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
            path: NonEmpty::from((first.into(), rest.into_iter().map(Into::into).collect())),
            scope,
        }
    }
}
