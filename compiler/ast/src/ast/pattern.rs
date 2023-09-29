#[allow(clippy::wildcard_imports)]
use super::*;

use ordered_float::NotNan;

#[derive(Debug)]
pub enum Pattern {
    IntLiteral(IntLiteral),
    FractionLiteral(FractionLiteral),
    StringLiteral(StringLiteral),
    CharLiteral(CharLiteral),
    VariableRef(VariableRef),
    Destructor(Destructor),
}

impl Pattern {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral::cast(node)?),
            SyntaxKind::FractionLiteral => Self::FractionLiteral(FractionLiteral::cast(node)?),
            SyntaxKind::StringLiteral => Self::StringLiteral(StringLiteral::cast(node)?),
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral::cast(node)?),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef::cast(node)?),
            SyntaxKind::Destructor => Self::Destructor(Destructor::cast(node)?),
            _ => return None,
        };

        Some(result)
    }
}

pub struct IntLiteral(SyntaxNode);

impl IntLiteral {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::IntLiteral {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn value(&self) -> Option<u64> {
        self.0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .parse()
            .ok()
    }

    #[must_use]
    pub fn span(&self) -> TextRange {
        self.0.text_range()
    }
}

impl fmt::Debug for IntLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IntLiteral")
            .field("value", &self.value())
            .finish()
    }
}

pub struct FractionLiteral(SyntaxNode);

impl FractionLiteral {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::FractionLiteral {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn value(&self) -> Option<NotNan<f64>> {
        self.0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .parse()
            .ok()
    }
}

impl fmt::Debug for FractionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FractionLiteral")
            .field("value", &self.value())
            .finish()
    }
}

pub struct StringLiteral(SyntaxNode);

impl StringLiteral {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::StringLiteral {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn value(&self) -> String {
        let string = self
            .0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .to_string();

        string[1..string.len() - 1].to_string()
    }
}

impl fmt::Debug for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StringLiteral")
            .field("value", &self.value())
            .finish()
    }
}

pub struct CharLiteral(SyntaxNode);

impl CharLiteral {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::CharLiteral {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn value(&self) -> Option<char> {
        self.0
            .first_token()
            .iter()
            .filter(|token| token.text().len() == 3)
            .find_map(|token| token.text().chars().nth(1))
    }

    #[must_use]
    pub fn span(&self) -> TextRange {
        self.0.text_range()
    }
}

impl fmt::Debug for CharLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CharLiteral")
            .field("value", &self.value())
            .finish()
    }
}

pub struct VariableRef(SyntaxNode);

impl VariableRef {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::VariableRef {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn name(&self) -> Option<Path> {
        Path::cast(self.0.clone())
    }
}

impl fmt::Debug for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VariableRef")
            .field("name", &self.name())
            .finish()
    }
}

pub struct Destructor(SyntaxNode);

impl Destructor {
    #[must_use]
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Destructor {
            Some(Self(node))
        } else {
            None
        }
    }

    #[must_use]
    pub fn target(&self) -> Option<Path> {
        self.0.children().find_map(|token| {
            if token.kind() == SyntaxKind::DestructorTarget {
                token.children().find_map(Path::cast)
            } else {
                None
            }
        })
    }

    pub fn args(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(|token| {
            if token.kind() == SyntaxKind::DestructorArg {
                token.children().find_map(Pattern::cast)
            } else {
                None
            }
        })
    }
}

impl fmt::Debug for Destructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Destructor")
            .field("target", &self.target())
            .field("args", &self.args().collect::<Vec<_>>())
            .finish()
    }
}
