use non_empty_vec::NonEmpty;
use std::fmt;

mod ast_glossary;
mod hir;

pub use hir::*;

mod index;
#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirDatabase: alloy_workspace::WorkspaceDatabase {}

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fqn {
    pub module: NonEmpty<Name>,
    pub name: Name,
    pub sub_path: Option<Name>,
}

impl Fqn {
    #[inline]
    pub fn new(
        module: impl IntoIterator<Item = impl Into<Name>>,
        local_name: impl Into<Name>,
        sub_path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Self {
        let mut raw_sub_path: Vec<Name> = sub_path.into_iter().map(Into::into).collect();
        let sub_path_last = raw_sub_path.pop();

        let (module, name, sub_path) = {
            let mut module_segments: Vec<Name> = vec![];
            for segment in module.into_iter() {
                module_segments.push(segment.into());
            }
            let local_name = local_name.into();

            // Build additional segments from raw_sub_path (not including the last element which is in sub_path_last)
            for segment in &raw_sub_path {
                module_segments.push(segment.clone());
            }

            if module_segments.is_empty() && sub_path_last.is_some() {
                module_segments.push(local_name);
                let module = unsafe { NonEmpty::new_unchecked(module_segments) };
                (module, sub_path_last.unwrap(), None)
            }
            // Normal case: module is not empty, keep sub_path as-is
            else {
                let module = unsafe { NonEmpty::new_unchecked(module_segments) };
                (module, local_name, sub_path_last)
            }
        };

        Self {
            module,
            name,
            sub_path,
        }
    }

    pub fn module_slug(&self) -> String {
        self.module
            .iter()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn segments(&self) -> NonEmpty<Name> {
        let mut segments = unsafe { NonEmpty::new_unchecked(vec![]) };
        for segment in self.module.iter() {
            segments.push(segment.clone());
        }
        segments.push(self.name.clone());
        if let Some(segment) = &self.sub_path {
            segments.push(segment.clone());
        }
        segments
    }
}
