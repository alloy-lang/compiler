use non_empty_vec::NonEmpty;

mod hir;
mod index;
#[cfg(test)]
mod tests;

pub use hir::lower_repl_line;
pub use hir::lower_source_file;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

impl Name {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

#[derive(Debug, PartialEq)]
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
}
