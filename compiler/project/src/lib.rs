use camino::{Utf8Path, Utf8PathBuf};
use std::fs;

use serde::Deserialize;
use walkdir::WalkDir;

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
    #[serde(alias = "lib")]
    Library,
    #[serde(alias = "bin")]
    Binary,
}

#[derive(Debug)]
pub struct Project {
    modules: Vec<ModuleFile>,
    config: ProjectConfig,
}

#[derive(Debug)]
pub struct ModuleFile {
    slug: String,
    path: Utf8PathBuf,
    contents: String,
}

impl ModuleFile {
    #[must_use]
    pub fn path(&self) -> &Utf8Path {
        &self.path
    }

    #[must_use]
    pub fn contents(&self) -> &str {
        &self.contents
    }
}

#[derive(Debug)]
pub struct ProjectError {
    kind: ProjectErrorKind,
}

#[derive(Debug)]
pub enum ProjectErrorKind {
    MissingAlloyToml(Utf8PathBuf),
    MissingSourceDirectory(Utf8PathBuf),
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
    pub fn new(root: impl Into<Utf8PathBuf>) -> Result<Self, ProjectError> {
        let root = root.into();
        let alloy_toml_path = root.join("Alloy.toml");
        let source_root = root.join("src");

        let Ok(raw_project_config) = fs::read_to_string(&alloy_toml_path) else {
            return Err(ProjectError {
                kind: ProjectErrorKind::MissingAlloyToml(alloy_toml_path),
            });
        };
        let config: ProjectConfig = toml::from_str(&raw_project_config)?;

        let Ok(_) = fs::read_dir(&source_root) else {
            return Err(ProjectError {
                kind: ProjectErrorKind::MissingSourceDirectory(source_root),
            });
        };

        let modules = Self::build_module_files(&config.package.name, &source_root);

        Ok(Self { modules, config })
    }

    fn build_module_files(package_name: &str, source_root: &Utf8PathBuf) -> Vec<ModuleFile> {
        let mut modules = Vec::new();

        for entry in WalkDir::new(source_root).follow_links(true) {
            let Ok(entry) = entry else {
                continue;
            };
            let Some(path) = Utf8Path::from_path(entry.path()) else {
                continue;
            };

            let trimmed_path = {
                if !path.is_file() {
                    continue;
                }
                if Some("alloy") != path.extension() {
                    continue;
                }
                path.strip_prefix(source_root)
                    .expect("expected root path to be a prefix")
            };

            let module_slug = {
                let raw_module_path_formatted = trimmed_path
                    .iter()
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>()
                    .join("::");

                format!(
                    "{}::{}",
                    package_name,
                    raw_module_path_formatted.trim_end_matches(".alloy")
                )
                .to_string()
            };

            let contents = fs::read_to_string(path).unwrap_or_else(|_| {
                panic!("Something went wrong reading the file '{:?}'", trimmed_path)
            });

            modules.push(ModuleFile {
                slug: module_slug,
                path: path.to_owned(),
                contents,
            });
        }
        modules
    }

    #[must_use]
    pub fn is_module(&self, path: &Utf8Path) -> bool {
        self.modules.iter().any(|m| m.path == path)
    }

    pub fn modules(&self) -> impl Iterator<Item = &ModuleFile> {
        self.modules.iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Project, ProjectConfigPackageType, ProjectErrorKind};

    #[test]
    fn test_std_lib() {
        let project = Project::new("../../std").expect("expected project to be created");

        assert_eq!(project.config.package.name, "std");
        assert_eq!(project.config.package.version, "0.0.1");
        assert_eq!(
            project.config.package.type_,
            ProjectConfigPackageType::Library
        );
        assert_eq!(
            project
                .modules()
                .map(|module| module.slug.clone())
                .collect::<Vec<_>>(),
            vec![
                "std::applicative",
                "std::bool",
                "std::function",
                "std::debug",
                "std::monad",
                "std::eq",
                "std::functor",
                "std::either",
                "std::option",
                "std::order",
            ],
        );
    }

    #[test]
    fn test_ignores_non_alloy_files() {
        let project = Project::new("./test_projects/ignores_non_alloy_files")
            .expect("expected project to be created");

        assert_eq!(project.config.package.name, "ignores_non_alloy_files");
        assert_eq!(project.config.package.version, "0.0.1");
        assert_eq!(
            project.config.package.type_,
            ProjectConfigPackageType::Library
        );
        assert_eq!(
            project
                .modules()
                .map(|module| module.slug.clone())
                .collect::<Vec<_>>(),
            vec!["ignores_non_alloy_files::one_file"],
        );
    }

    #[test]
    fn test_recursive_src() {
        let project =
            Project::new("./test_projects/recursive_src").expect("expected project to be created");

        assert_eq!(project.config.package.name, "recursive_src");
        assert_eq!(project.config.package.version, "0.0.1");
        assert_eq!(
            project.config.package.type_,
            ProjectConfigPackageType::Library
        );
        assert_eq!(
            project
                .modules()
                .map(|module| module.slug.clone())
                .collect::<Vec<_>>(),
            vec!["recursive_src::sub::two_file", "recursive_src::one_file",],
        );
    }

    #[test]
    fn test_missing_all() {
        let project = Project::new("./test_projects/missing_all")
            .expect_err("expected project to be missing");

        let ProjectErrorKind::MissingAlloyToml(path): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(path.as_str(), "./test_projects/missing_all/Alloy.toml");
    }

    #[test]
    fn test_missing_source() {
        let project = Project::new("./test_projects/missing_source")
            .expect_err("expected project to be missing");

        let ProjectErrorKind::MissingSourceDirectory(path): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(path.as_str(), "./test_projects/missing_source/src");
    }

    #[test]
    fn test_incorrect_package_type() {
        let project = Project::new("./test_projects/incorrect_package_type")
            .expect_err("expected project to be missing");

        let ProjectErrorKind::Toml(toml_error): ProjectErrorKind = project.kind else {
            panic!("expected io error, was {:?}", project.kind);
        };

        assert_eq!(
            toml_error.message(),
            "unknown variant `fake`, expected `Library` or `Binary`"
        );
    }
}
