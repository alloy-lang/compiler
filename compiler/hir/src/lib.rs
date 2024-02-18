use std::fmt;
use non_empty_vec::NonEmpty;

mod hir;
mod index;
#[cfg(test)]
mod tests;

pub use hir::lower_repl_line;
pub use hir::lower_source_file;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl PartialEq<&str> for Name {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl Name {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fqn {
    pub module: NonEmpty<Name>,
    pub name: Name,
}

impl Fqn {
    #[inline]
    pub fn new(
        module: impl IntoIterator<Item = impl Into<String>>,
        name: impl Into<String>,
    ) -> Self {
        unsafe {
            Self {
                module: NonEmpty::new_unchecked(module.into_iter().map(Name::new).collect()),
                name: Name::new(name.into()),
            }
        }
    }

    fn first_module_segment(&self) -> &Name {
        self.module.first()
    }

    fn name(&self) -> &Name {
        &self.name
    }
}
