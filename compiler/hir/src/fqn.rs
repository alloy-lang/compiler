use crate::{HirDatabase, Name};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fqn {
    pub module_id: ModuleId,
    pub module: NonEmpty<Name>,
    pub name: Name,
    pub sub_path: Option<Name>,
}

impl Fqn {
    pub fn resolve(
        db: &dyn HirDatabase,
        module: impl IntoIterator<Item = impl Into<Name>>,
        local_name: impl Into<Name>,
        sub_path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Result<Self, NonEmpty<Name>> {
        let module_segments: Vec<Name> = module.into_iter().map(Into::into).collect();
        let local_name: Name = local_name.into();
        let sub_path_segments: Vec<Name> = sub_path.into_iter().map(Into::into).collect();
        let has_sub_path = !sub_path_segments.is_empty();

        // Flatten everything into one path: [module..., local_name, sub_path...]
        let mut full_path = module_segments.clone();
        full_path.push(local_name.clone());
        full_path.extend(sub_path_segments);

        assert!(!full_path.is_empty(), "Cannot create Fqn with empty path");

        // Try each split from longest module prefix to shortest
        let mut best_module_match: Option<NonEmpty<Name>> = None;

        for split in (1..full_path.len()).rev() {
            let (module_path, rest) = full_path.split_at(split);
            let item_name = &rest[0];
            let remaining = &rest[1..];

            let slug = module_path
                .iter()
                .map(Name::as_str)
                .collect::<Vec<_>>()
                .join("::");

            let Some(module_id) = db.find_module_by_slug(&slug) else {
                continue;
            };

            // Valid: remaining fits in sub_path (0 or 1 segment)
            if remaining.len() <= 1 {
                return Ok(Fqn {
                    module_id,
                    module: unsafe { NonEmpty::new_unchecked(module_path.to_vec()) },
                    name: item_name.clone(),
                    sub_path: remaining.first().cloned(),
                });
            }

            // Module exists but too many remaining segments — remember as best guess
            if best_module_match.is_none() {
                best_module_match = Some(unsafe { NonEmpty::new_unchecked(module_path.to_vec()) });
            }
        }

        let default_guess = {
            let mut guess = module_segments;
            // if sub_path was provided, assume local_name is part of the module path
            if has_sub_path {
                guess.push(local_name);
            }
            unsafe { NonEmpty::new_unchecked(guess) }
        };

        Err(best_module_match.unwrap_or(default_guess))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirDatabase;
    use salsa::Database;

    fn ne_vec(segments: Vec<impl Into<Name>>) -> NonEmpty<Name> {
        unsafe { NonEmpty::new_unchecked(segments.into_iter().map(Into::into).collect()) }
    }

    #[test]
    fn test_does_not_shift_into_module_when_subpath_empty() {
        let mut db = TestHirDatabase::default();
        let test_module_id = db.add_test_module("a::b::c", "");

        let Fqn {
            module_id,
            module,
            name,
            sub_path,
        } = Fqn::resolve(&db, ["a", "b", "c"], "d", [] as [String; 0]).unwrap();

        db.attach(|_| {
            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c"]));
            assert_eq!(name, Name::new("d"));
            assert_eq!(sub_path, None);
        });
    }

    #[test]
    fn test_does_not_shift_into_module_when_subpath_has_more_1() {
        let mut db = TestHirDatabase::default();
        let test_module_id = db.add_test_module("a::b::c", "");

        let Fqn {
            module_id,
            module,
            name,
            sub_path,
        } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e"]).unwrap();

        db.attach(|_| {
            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c"]));
            assert_eq!(name, Name::new("d"));
            assert_eq!(sub_path, Some(Name::new("e")));
        });
    }

    #[test]
    fn test_shifts_into_module_when_short_module_is_not_found() {
        let mut db = TestHirDatabase::default();
        let test_module_id = db.add_test_module("a::b::c::d", "");

        let Fqn {
            module_id,
            module,
            name,
            sub_path,
        } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e"]).unwrap();

        db.attach(|_| {
            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c", "d"]));
            assert_eq!(name, Name::new("e"));
            assert_eq!(sub_path, None);
        });
    }

    #[test]
    fn test_shifts_into_module_when_subpath_has_more_than_1() {
        let mut db = TestHirDatabase::default();
        let test_module_id = db.add_test_module("a::b::c::d", "");

        let Fqn {
            module_id,
            module,
            name,
            sub_path,
        } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap();

        db.attach(|_| {
            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c", "d"]));
            assert_eq!(name, Name::new("e"));
            assert_eq!(sub_path, Some(Name::new("f")));
        });
    }

    #[test]
    fn test_returns_best_module_path_guess_when_unable_to_find() {
        let db = TestHirDatabase::default();

        let err_module_path = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap_err();

        assert_eq!(err_module_path, ne_vec(vec!["a", "b", "c", "d"]));
    }

    #[test]
    fn test_returns_best_module_path_guess_when_more_than_one_subpath() {
        let mut db = TestHirDatabase::default();
        let test_module_id = db.add_test_module("a::b", "");

        let err_module_path = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap_err();

        assert_eq!(err_module_path, ne_vec(vec!["a", "b"]));
    }
}
