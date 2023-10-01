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
        self.0.first_token()?.text().parse().ok()
    }
}

ast_node!(FractionLiteral, fields: [value]);

impl FractionLiteral {
    #[must_use]
    pub fn value(&self) -> Option<NotNan<f64>> {
        self.0.first_token()?.text().parse().ok()
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
        first_matching_child(self, SyntaxKind::DestructorTarget)
    }

    #[must_use]
    pub fn args(&self) -> Vec<Pattern> {
        all_matching_children(self, SyntaxKind::DestructorArg)
    }
}
