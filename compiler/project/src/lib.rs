use std::fs;
use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;
use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct ProjectConfig {
    package: ProjectConfigPackage,
}
#[derive(Deserialize, Debug)]
pub struct ProjectConfigPackage {
    name: String,
    version: String,
    #[serde(alias = "type")]
    type_: ProjectConfigPackageType,
}
#[derive(Deserialize, Debug, PartialEq)]
pub enum ProjectConfigPackageType {
    #[serde(alias = "library")]
    Library,
    #[serde(alias = "executable")]
    Executable,
}

#[derive(Debug)]
pub struct Project {
    modules: FxHashMap<String, PathBuf>,
    config: ProjectConfig,
}

#[derive(Debug)]
pub struct ProjectError {
    kind: ProjectErrorKind,
}

#[derive(Debug)]
pub enum ProjectErrorKind {
    MissingAlloyToml(PathBuf),
    MissingSourceDirectory(PathBuf),
    Toml(toml::de::Error),
    MissingRootModule(ProjectConfigPackageType),
}

impl From<toml::de::Error> for ProjectError {
    fn from(error: toml::de::Error) -> Self {
        Self {
            kind: ProjectErrorKind::Toml(error),
        }
    }
}

impl Project {
    pub fn new(root: &Path) -> Result<Self, ProjectError> {
        let alloy_toml_path = root.join("Alloy.toml");
        let source_path = root.join("src");

        let Ok(raw_project_config) = fs::read_to_string(&alloy_toml_path) else {
            return Err(ProjectError {
                kind: ProjectErrorKind::MissingAlloyToml(alloy_toml_path),
            });
        };
        let config: ProjectConfig = toml::from_str(&raw_project_config)?;

        let mut modules = FxHashMap::default();

        let Ok(module_files) = fs::read_dir(&source_path) else {
            return Err(ProjectError {
                kind: ProjectErrorKind::MissingSourceDirectory(source_path),
            });
        };

        for entry in module_files {
            let Ok(entry) = entry else {
                continue;
            };
            let Ok(metadata) = entry.metadata() else {
                continue;
            };

            if !metadata.is_file() {
                continue;
            }

            let file_name = entry.file_name();
            let Some(file_name) = file_name.to_str() else {
                continue;
            };

            let Some((module_name, "alloy")) = file_name.rsplit_once('.') else {
                continue;
            };

            modules.insert(module_name.to_string(), entry.path());
        }

        Ok(Self { modules, config })
    }

    #[must_use]
    pub fn is_module(&self, path: &Path) -> bool {
        self.modules.values().any(|p| p == path)
    }

    pub fn modules(&self) -> impl Iterator<Item = (&String, &Path)> {
        self.modules
            .iter()
            .map(|(name, path)| (name, path.as_path()))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{Project, ProjectConfigPackageType, ProjectErrorKind};

    #[test]
    fn test_std_lib() {
        let project = Project::new(Path::new("../../std")).expect("expected project to be created");

        assert_eq!(project.config.package.name, "std");
        assert_eq!(project.config.package.version, "0.0.1");
        assert_eq!(project.config.package.type_, ProjectConfigPackageType::Library);
        assert_eq!(
            project.modules().map(|(name, _)| name).collect::<Vec<_>>(),
            vec![
                "monad",
                "functor",
                "function",
                "bool",
                "either",
                "option",
                "order",
                "eq",
                "applicative",
                "debug"
            ],
        );
    }

    #[test]
    fn test_missing_all() {
        let project = Project::new(Path::new("./test_projects/missing_all"))
            .expect_err("expected project to be missing");

        let ProjectErrorKind::MissingAlloyToml(path): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(
            path.to_str(),
            Some("./test_projects/missing_all/Alloy.toml")
        );
    }

    #[test]
    fn test_missing_source() {
        let project = Project::new(Path::new("./test_projects/missing_source"))
            .expect_err("expected project to be missing");

        let ProjectErrorKind::MissingSourceDirectory(path): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(path.to_str(), Some("./test_projects/missing_source/src"));
    }

    #[test]
    fn test_incorrect_package_type() {
        let project = Project::new(Path::new("./test_projects/incorrect_package_type"))
            .expect_err("expected project to be missing");

        let ProjectErrorKind::Toml(toml_error): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(toml_error.message(), "unknown variant `fake`, expected `Library` or `Executable`");
    }
}
