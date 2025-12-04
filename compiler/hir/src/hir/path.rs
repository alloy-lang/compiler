#[allow(clippy::wildcard_imports)]
use super::*;
use std::fmt;

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

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Path::ThisModule { name, subname, .. } => vec![name]
                .into_iter()
                .chain(subname.iter())
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join("::")
                .fmt(f),
            Path::OtherModule(fqn) => fqn
                .segments()
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join("::")
                .fmt(f),
            Path::Unknown(names) => names
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join("::")
                .fmt(f),
        }
    }
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
