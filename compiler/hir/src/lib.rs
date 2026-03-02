use std::fmt;

mod ast_glossary;
mod diagnostics;
mod hir;
mod hir_module;

pub use diagnostics::*;
pub use hir::*;
pub use hir_module::HirModule;

mod fqn;
pub use fqn::{Fqn, FqnResolutionError};
mod index;
#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirDatabase: alloy_ast::AstDatabase {}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<&str> for Name {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl Name {
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        let name = name.trim_start_matches('(').trim_end_matches(')');

        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        Self(name)
    }
}

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        Self(name.to_string())
    }
}

impl From<&String> for Name {
    fn from(name: &String) -> Self {
        Self(name.to_string())
    }
}
