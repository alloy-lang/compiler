#[allow(clippy::wildcard_imports)]
use super::*;

pub struct Import(SyntaxNode);

impl Import {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ImportStatement {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn path(&self) -> Vec<String> {
        let (_last, rest) = Self::full_path(self);

        rest.into_iter()
            .filter_map(|child| {
                if let ImportChild::Segment(segment) = child {
                    segment.name()
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn targets(&self) -> Vec<String> {
        let (last, _) = Self::full_path(self);

        match last {
            ImportChild::Segment(segment) => vec![segment.name().unwrap()],
            ImportChild::Group(group) => group.names(),
        }
    }

    fn full_path(&self) -> (ImportChild, Vec<ImportChild>) {
        let mut full_path = self
            .0
            .children()
            .filter_map(ImportChild::cast)
            .collect::<Vec<_>>();

        let last = full_path.pop().unwrap();

        (last, full_path)
    }
}

impl fmt::Debug for Import {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Import")
            .field("path", &self.path())
            .field("targets", &self.targets())
            .finish()
    }
}

#[derive(Debug)]
pub enum ImportChild {
    Segment(ImportChildSegment),
    Group(ImportChildGroup),
}

impl ImportChild {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ImportStatementSegment {
            Some(Self::Segment(ImportChildSegment(node)))
        } else if node.kind() == SyntaxKind::ImportStatementGroup {
            Some(Self::Group(ImportChildGroup(node)))
        } else {
            None
        }
    }
}

pub struct ImportChildSegment(SyntaxNode);

impl ImportChildSegment {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::ImportStatementSegment {
            Some(ImportChildSegment(node))
        } else {
            None
        }
    }

    fn name(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|ct| ct.kind() == SyntaxKind::Ident)
            .map(|token| token.text().into())
    }
}

impl fmt::Debug for ImportChildSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImportChildSegment")
            .field("name", &self.name())
            .finish()
    }
}

pub struct ImportChildGroup(SyntaxNode);

impl ImportChildGroup {
    fn names(&self) -> Vec<String> {
        self.0
            .children()
            .filter_map(ImportChildSegment::cast)
            .filter_map(|token| token.name())
            .collect()
    }
}

impl fmt::Debug for ImportChildGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImportChildGroup")
            .field("names", &self.names())
            .finish()
    }
}
