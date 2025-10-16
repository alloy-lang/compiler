use non_empty_vec::NonEmpty;
use std::fmt;

mod ast_glossary;
mod hir;
mod index;
#[cfg(test)]
mod tests;

pub use hir::lower_source_file;

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fqn {
    // todo: consider replacing module path with the module id
    module: NonEmpty<Name>,
    name: Name,
    sub_path: Vec<Name>,
}

impl Fqn {
    #[inline]
    pub fn new(
        module: impl IntoIterator<Item = impl Into<Name>>,
        name: impl Into<Name>,
        path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Self {
        unsafe {
            Self {
                module: NonEmpty::new_unchecked(module.into_iter().map(Into::into).collect()),
                name: name.into(),
                sub_path: path.into_iter().map(Into::into).collect(),
            }
        }
    }
}
