use crate::{HirDefDatabase, Name};
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, Severity};
use alloy_workspace::{ModuleId, VirtualModuleId};
use itertools::Itertools;
use non_empty_vec::NonEmpty;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FqnResolutionError {
    UnknownRootModule {
        attempted_module_path: NonEmpty<Name>,
    },
    UnknownChildModule {
        module_id: VirtualModuleId,
        unknown_child: Name,
        available_child_modules: Vec<ModuleId>,
    },
    MissingLocalName {
        module_id: ModuleId,
    },
    ExtraSegments {
        fqn: Fqn,
        extra_segments: NonEmpty<Name>,
    },
}

impl Diagnostic for FqnResolutionError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match self {
            FqnResolutionError::UnknownRootModule { .. } => Some("E21001"),
            FqnResolutionError::UnknownChildModule { .. } => Some("E21002"),
            FqnResolutionError::MissingLocalName { .. } => Some("E21003"),
            FqnResolutionError::ExtraSegments { .. } => Some("E21004"),
        }
    }

    fn message(&self) -> String {
        match self {
            FqnResolutionError::UnknownRootModule {
                attempted_module_path,
            } => {
                format!(
                    "Cannot find module '{}'",
                    attempted_module_path
                        .iter()
                        .map(Name::as_str)
                        .collect::<Vec<_>>()
                        .join("::")
                )
            }
            FqnResolutionError::UnknownChildModule {
                module_id,
                unknown_child,
                ..
            } => {
                format!(
                    "Module '{}' does not contain child module '{}'",
                    module_id, unknown_child,
                )
            }
            FqnResolutionError::MissingLocalName { module_id } => {
                format!("Module '{}' found but missing local name", module_id)
            }
            FqnResolutionError::ExtraSegments {
                fqn,
                extra_segments,
            } => {
                format!(
                    "Module '{}' found but extra path segments '{}' were not resolved",
                    fqn.module_id,
                    extra_segments
                        .iter()
                        .map(Name::as_str)
                        .collect::<Vec<_>>()
                        .join("::")
                )
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        unreachable!("caller should track range")
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match self {
            FqnResolutionError::UnknownRootModule {
                attempted_module_path,
            } => builder
                .with_primary_label(format!(
                    "Cannot find module '{}'",
                    attempted_module_path
                        .iter()
                        .map(Name::as_str)
                        .collect::<Vec<_>>()
                        .join("::")
                ))
                .with_help("Make sure the module path is correct"),
            FqnResolutionError::UnknownChildModule {
                module_id,
                unknown_child,
                available_child_modules,
            } => builder
                .with_primary_label(format!(
                    "module '{}' does not contain child module '{}'",
                    module_id, unknown_child,
                ))
                .with_help(format!(
                    "Module '{}' has the following child modules: {}",
                    module_id,
                    available_child_modules.iter().join(", ")
                )),
            FqnResolutionError::MissingLocalName { module_id } => builder
                .with_primary_label(format!(
                    "module '{}' found but missing local name",
                    module_id
                ))
                .with_help(format!(
                    "Trying to import a specific item from module '{}'",
                    module_id
                )),
            FqnResolutionError::ExtraSegments {
                fqn,
                extra_segments,
            } => {
                let extra_slug = extra_segments
                    .iter()
                    .map(Name::as_str)
                    .collect::<Vec<_>>()
                    .join("::");

                builder
                        .with_primary_label(format!("module '{}' found but extra path segments '{}' were not resolved", fqn.module_id, extra_slug))
                        .with_help(format!("Module '{}' was found, but the path segments '{}' could not be resolved within it", fqn.module_id, extra_slug))
            }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fqn {
    pub module_id: ModuleId,
    pub module: NonEmpty<Name>,
    pub name: Name,
    pub sub_path: Option<Name>,
}

impl Fqn {
    pub fn resolve(
        db: &dyn HirDefDatabase,
        module: impl IntoIterator<Item = impl Into<Name>>,
        local_name: impl Into<Name>,
        sub_path: impl IntoIterator<Item = impl Into<Name>>,
    ) -> Result<Self, FqnResolutionError> {
        let module_segments: Vec<Name> = module.into_iter().map(Into::into).collect();
        let local_name: Name = local_name.into();
        let sub_path_segments: Vec<Name> = sub_path.into_iter().map(Into::into).collect();

        // Flatten everything into one path: [module..., local_name, sub_path...]
        let mut full_path = module_segments.clone();
        full_path.push(local_name.clone());
        full_path.extend(sub_path_segments);

        assert!(
            full_path.len() > 1,
            "Cannot create Fqn with less than two path segments"
        );

        {
            let slug = full_path
                .iter()
                .map(Name::as_str)
                .collect::<Vec<_>>()
                .join("::");

            if let Some(module_id) = db.find_module_by_slug(&slug) {
                return Err(FqnResolutionError::MissingLocalName { module_id });
            }
        }

        for split in (1..full_path.len()).rev() {
            let (module_path, rest) = full_path.split_at(split);
            let item_name = &rest[0];
            let remaining = &rest[1..];

            let slug = module_path
                .iter()
                .map(Name::as_str)
                .collect::<Vec<_>>()
                .join("::");

            if let Some(module_id) = db.find_module_by_slug(&slug) {
                let fqn = Fqn {
                    module_id,
                    module: unsafe { NonEmpty::new_unchecked(module_path.to_vec()) },
                    name: item_name.clone(),
                    sub_path: remaining.first().cloned(),
                };

                return if remaining.len() <= 1 {
                    Ok(fqn)
                } else {
                    Err(FqnResolutionError::ExtraSegments {
                        fqn,
                        extra_segments: unsafe { NonEmpty::new_unchecked(remaining[1..].to_vec()) },
                    })
                };
            }

            if let Some(module_id) = db.find_virtual_module_by_slug(&slug) {
                let virtual_source = db.get_virtual_source(module_id);
                let available_child_modules = virtual_source.children.clone();

                return Err(FqnResolutionError::UnknownChildModule {
                    module_id,
                    unknown_child: item_name.clone(),
                    available_child_modules,
                });
            }
        }

        let shortest_module_path = full_path.first().cloned().unwrap();
        Err(FqnResolutionError::UnknownRootModule {
            attempted_module_path: NonEmpty::new(shortest_module_path.clone()),
        })
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
    use crate::tests::TestHirDefDatabase;
    use alloy_workspace::WorkspaceDatabase;
    use salsa::Database;

    fn ne_vec(segments: Vec<impl Into<Name>>) -> NonEmpty<Name> {
        unsafe { NonEmpty::new_unchecked(segments.into_iter().map(Into::into).collect()) }
    }

    #[test]
    fn test_does_not_shift_into_module_when_subpath_empty() {
        let mut db = TestHirDefDatabase::default();
        let test_module_id = db.add_test_module("a::b::c", "");

        db.attach(|_| {
            let Fqn {
                module_id,
                module,
                name,
                sub_path,
            } = Fqn::resolve(&db, ["a", "b", "c"], "d", [] as [String; 0]).unwrap();

            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c"]));
            assert_eq!(name, Name::new("d"));
            assert_eq!(sub_path, None);
        });
    }

    #[test]
    fn test_does_not_shift_into_module_when_subpath_has_1() {
        let mut db = TestHirDefDatabase::default();
        let test_module_id = db.add_test_module("a::b::c", "");

        db.attach(|_| {
            let Fqn {
                module_id,
                module,
                name,
                sub_path,
            } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e"]).unwrap();

            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c"]));
            assert_eq!(name, Name::new("d"));
            assert_eq!(sub_path, Some(Name::new("e")));
        });
    }

    #[test]
    fn test_shifts_into_module_when_short_module_is_not_found() {
        let mut db = TestHirDefDatabase::default();
        let test_module_id = db.add_test_module("a::b::c::d", "");

        db.attach(|_| {
            let Fqn {
                module_id,
                module,
                name,
                sub_path,
            } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e"]).unwrap();

            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c", "d"]));
            assert_eq!(name, Name::new("e"));
            assert_eq!(sub_path, None);
        });
    }

    #[test]
    fn test_shifts_into_module_when_subpath_has_more_than_1() {
        let mut db = TestHirDefDatabase::default();
        let test_module_id = db.add_test_module("a::b::c::d", "");

        db.attach(|_| {
            let Fqn {
                module_id,
                module,
                name,
                sub_path,
            } = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap();

            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c", "d"]));
            assert_eq!(name, Name::new("e"));
            assert_eq!(sub_path, Some(Name::new("f")));
        });
    }

    #[test]
    fn test_matches_longest_module() {
        let mut db = TestHirDefDatabase::default();
        let _ = db.add_test_module("a", "");
        let _ = db.add_test_module("a::b", "");
        let _ = db.add_test_module("a::b::c", "");
        let _ = db.add_test_module("a::b::c::e", "");
        let test_module_id = db.add_test_module("a::b::c::d", "");

        db.attach(|_| {
            let Fqn {
                module_id,
                module,
                name,
                sub_path,
            } = Fqn::resolve(&db, ["a"], "b", ["c", "d", "e", "f"]).unwrap();

            assert_eq!(test_module_id, module_id);
            assert_eq!(module, ne_vec(vec!["a", "b", "c", "d"]));
            assert_eq!(name, Name::new("e"));
            assert_eq!(sub_path, Some(Name::new("f")));
        });
    }

    #[test]
    fn test_err_when_unable_to_find_root_module() {
        let db = TestHirDefDatabase::default();

        let expected_err = FqnResolutionError::UnknownRootModule {
            attempted_module_path: ne_vec(vec!["a"]),
        };

        let err = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap_err();
        assert_eq!(err, expected_err);

        let err = Fqn::resolve(&db, ["a", "b", "c", "d"], "e", ["f"]).unwrap_err();
        assert_eq!(err, expected_err);

        let err = Fqn::resolve(&db, ["a", "b", "c", "d", "e"], "f", [] as [String; 0]).unwrap_err();
        assert_eq!(err, expected_err);
    }

    #[test]
    fn test_err_when_module_is_found_but_extra_subpath() {
        let mut db = TestHirDefDatabase::default();
        db.add_test_module("a::b", "");

        let expected_err = FqnResolutionError::ExtraSegments {
            fqn: Fqn {
                module_id: db.find_module_by_slug("a::b").unwrap(),
                module: ne_vec(vec!["a", "b"]),
                name: Name::new("c"),
                sub_path: Some(Name::new("d")),
            },
            extra_segments: ne_vec(vec!["e", "f"]),
        };

        db.attach(|_| {
            let err = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap_err();
            assert_eq!(err, expected_err);
        });
    }

    #[test]
    fn test_err_when_parent_module_is_found_but_not_child() {
        let mut db = TestHirDefDatabase::default();
        let test_module_id = db.add_test_module("a::b::c::d::jk", "");

        let expected_err = FqnResolutionError::UnknownChildModule {
            module_id: VirtualModuleId::new(&db, "a::b::c::d".to_string()),
            unknown_child: Name::new("e"),
            available_child_modules: vec![test_module_id],
        };

        db.attach(|_| {
            let err = Fqn::resolve(&db, ["a", "b", "c"], "d", ["e", "f"]).unwrap_err();
            assert_eq!(err, expected_err);
        });
    }
}
