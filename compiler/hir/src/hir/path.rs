#[allow(clippy::wildcard_imports)]
use super::*;

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
    UnknownReference(NonEmpty<Name>),
    UnresolvedModule(FqnResolutionError),
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

    pub fn resolution_kinds(&self) -> Vec<ResolutionKind> {
        match self {
            Path::ThisModule {
                resolution_kind, ..
            } => {
                vec![resolution_kind.clone()]
            }
            Path::OtherModule(_, resolution_kinds) => resolution_kinds.clone(),
            Path::UnknownReference(_) => {
                vec![]
            }
            Path::UnresolvedModule(_) => {
                vec![]
            }
        }
    }
}
