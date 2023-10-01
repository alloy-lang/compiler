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
    NilIdentifier(NilIdentifier),
    Destructor(Destructor),
}

impl Pattern {
    #[must_use]
    pub(crate) fn cast(node: SyntaxElement) -> Option<Self> {
        let result = match node.kind() {
            SyntaxKind::IntLiteral => Self::IntLiteral(IntLiteral::cast(node.into_node()?)?),
            SyntaxKind::FractionLiteral => {
                Self::FractionLiteral(FractionLiteral::cast(node.into_node()?)?)
            }
            SyntaxKind::StringLiteral => {
                Self::StringLiteral(StringLiteral::cast(node.into_node()?)?)
            }
            SyntaxKind::CharLiteral => Self::CharLiteral(CharLiteral::cast(node.into_node()?)?),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef::cast(node.into_node()?)?),
            SyntaxKind::NilIdentifier => {
                Self::NilIdentifier(NilIdentifier::cast(node.into_token()?)?)
            }
            SyntaxKind::Destructor => Self::Destructor(Destructor::cast(node.into_node()?)?),
            _ => return None,
        };

        Some(result)
    }
}

ast_node!(IntLiteral, fields: [value]);

impl IntLiteral {
    #[must_use]
    pub fn value(&self) -> Option<u64> {
        self.0
            .first_token()
            .expect("first_token will always exist")
            .text()
            .parse()
            .ok()
    }
}

ast_node!(FractionLiteral, fields: [value]);

impl FractionLiteral {
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

ast_node!(StringLiteral, fields: [value]);

impl StringLiteral {
    #[must_use]
    pub fn value(&self) -> Option<String> {
        let string = self.0.first_token()?.text().to_string();

        Some(string[1..string.len() - 1].to_string())
    }
}

ast_node!(CharLiteral, fields: [value]);

impl CharLiteral {
    #[must_use]
    pub fn value(&self) -> Option<char> {
        self.0
            .first_token()
            .iter()
            .filter(|token| token.text().len() == 3)
            .find_map(|token| token.text().chars().nth(1))
    }
}

ast_node!(VariableRef, fields: [name]);

impl VariableRef {
    #[must_use]
    pub fn name(&self) -> Option<Path> {
        first_child(self)
    }
}

ast_token!(NilIdentifier, fields: []);

ast_node!(Destructor, fields: [target, args]);

impl Destructor {
    #[must_use]
    pub fn target(&self) -> Option<VariableRef> {
        match_node(self, SyntaxKind::DestructorTarget)?
            .children()
            .find_map(VariableRef::cast)
    }

    pub fn args(&self) -> Vec<Pattern> {
        match_nodes(self, SyntaxKind::DestructorArg)
            .filter_map(|n| n.children_with_tokens().find_map(Pattern::cast))
            .collect()
    }
}
