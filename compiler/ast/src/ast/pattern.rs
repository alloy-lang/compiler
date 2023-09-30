#[allow(clippy::wildcard_imports)]
use super::*;

use ordered_float::NotNan;

ast_union_node!(Pattern, kinds: [
    IntLiteral,
    FractionLiteral,
    StringLiteral,
    CharLiteral,
    VariableRef,
    NilIdentifier,
    Destructor
]);

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

ast_node!(NilIdentifier, fields: []);

pub struct Destructor(SyntaxNode);
impl AstNode for Destructor {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Destructor {
            Some(Self(node))
        } else {
            None
        }
    }

    fn syntax(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl fmt::Debug for Destructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(Destructor))
            .field(stringify!(target), &self.target())
            .field(stringify!(args), &self.args())
            .finish()
    }
}

impl Destructor {
    #[must_use]
    pub fn target(&self) -> Option<VariableRef> {
        match_node(self, SyntaxKind::DestructorTarget)?
            .children()
            .find_map(VariableRef::cast)
    }

    pub fn args(&self) -> Vec<Pattern> {
        match_nodes(self, SyntaxKind::DestructorArg)
            .filter_map(|n| n.children().find_map(Pattern::cast))
            .collect()
    }
}
