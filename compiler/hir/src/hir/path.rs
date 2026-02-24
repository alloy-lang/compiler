#[allow(clippy::wildcard_imports)]
use super::*;
use std::fmt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Path {
    ThisModule {
        name: Name,
        subname: Option<Name>,
        scope: ScopeIdx,
        resolution_kind: ResolutionKind,
        resolution_idx: ResolutionIdx,
    },
    OtherModule(Fqn, Vec<ResolutionKind>),
    Unknown(NonEmpty<Name>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolutionKind {
    Trait,
    TypeDefinition,
    AbstractTraitMember,
    Expression,
    Pattern,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolutionIdx {
    Trait(TraitIdx),
    TypeDefinition(TypeDefinitionIdx),
    AbstractTraitMember(TypeIdx),
    Expression(ExpressionIdx),
    Unresolved,
    Pattern(PatternIdx),
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Path::ThisModule { name, subname, .. } => vec![name]
                .into_iter()
                .chain(subname.iter())
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("::")
                .fmt(f),
            Path::OtherModule(fqn, _) => fqn
                .segments()
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("::")
                .fmt(f),
            Path::Unknown(names) => names
                .iter()
                .map(ToString::to_string)
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
        resolution_kind: ResolutionKind,
        resolution_idx: ResolutionIdx,
    ) -> Self {
        Self::ThisModule {
            name: first.into(),
            subname: rest.into_iter().next().map(Into::into),
            scope,
            resolution_kind,
            resolution_idx,
        }
    }
}
