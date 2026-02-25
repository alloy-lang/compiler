use non_empty_vec::NonEmpty;
use std::fmt;

mod ast_glossary;
mod diagnostics;
mod hir;
mod hir_module;

pub use diagnostics::*;
pub use hir::*;
pub use hir_module::HirModule;

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
    pub fn new(
        module: impl IntoIterator<Item = impl Into<Name>>,
        local_name: impl Into<Name>,
        sub_path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Self {
        let sub_path_vec: Vec<Name> = sub_path.into_iter().map(Into::into).collect();
        let had_sub_path = !sub_path_vec.is_empty();

        // Build the full path from all segments
        let mut all_segments: Vec<Name> = module.into_iter().map(Into::into).collect();
        let local_name = local_name.into();
        all_segments.push(local_name);
        all_segments.extend(sub_path_vec);

        // We need at least 1 segment
        assert!(
            !all_segments.is_empty(),
            "Cannot create Fqn with empty path"
        );

        match all_segments.len() {
            // Single segment: use it for both module and name, no sub_path
            1 => {
                let name = all_segments[0].clone();
                let module = unsafe { NonEmpty::new_unchecked(all_segments) };
                Self {
                    module,
                    name,
                    sub_path: None,
                }
            }
            // Two segments: split as module=[first], name=second, sub_path=None
            2 => {
                let name = all_segments.pop().unwrap();
                let module = unsafe { NonEmpty::new_unchecked(all_segments) };
                Self {
                    module,
                    name,
                    sub_path: None,
                }
            }
            // Three or more: check if we had sub_path to decide how to split
            _ => {
                if had_sub_path {
                    // Had sub_path: split as module=[...], name=second-to-last, sub_path=last
                    let sub_path = all_segments.pop();
                    let name = all_segments.pop().unwrap();
                    let module = unsafe { NonEmpty::new_unchecked(all_segments) };
                    Self {
                        module,
                        name,
                        sub_path,
                    }
                } else {
                    // No sub_path: split as module=[...], name=last, sub_path=None
                    let name = all_segments.pop().unwrap();
                    let module = unsafe { NonEmpty::new_unchecked(all_segments) };
                    Self {
                        module,
                        name,
                        sub_path: None,
                    }
                }
            }
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
