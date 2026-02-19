//! Centralized diagnostic collection and reporting

use alloy_workspace::{ModuleId, SourceFile, WorkspaceDatabase};
use ariadne::{Config, Source};

use crate::builder::DiagnosticBuilder;
use crate::core::{Diagnostic, Severity};

/// A reporter for collecting and rendering diagnostics from multiple modules
///
/// This struct provides a centralized way to collect diagnostics from different
/// compiler phases and modules, then render them all together with proper source
/// context retrieved from the workspace database.
///
/// # Example
///
/// ```ignore
/// let mut reporter = DiagnosticsReporter::new();
///
/// // Collect diagnostics from type checking
/// for error in typed_module.errors() {
///     reporter.add(module_id, error.clone());
/// }
///
/// // Render all diagnostics
/// let output = reporter.render(&db);
/// println!("{}", output);
/// ```
#[derive(Default)]
pub struct DiagnosticsReporter {
    entries: Vec<DiagnosticEntry>,
}

struct DiagnosticEntry {
    module_id: ModuleId,
    diagnostic: Box<dyn Diagnostic>,
}

impl DiagnosticsReporter {
    /// Create a new empty diagnostics reporter
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Add a single diagnostic for a module
    pub fn add(&mut self, module_id: ModuleId, diagnostic: impl Diagnostic + 'static) {
        self.entries.push(DiagnosticEntry {
            module_id,
            diagnostic: Box::new(diagnostic),
        });
    }

    /// Add multiple diagnostics for a module
    pub fn add_all(
        &mut self,
        module_id: ModuleId,
        diagnostics: impl IntoIterator<Item = impl Diagnostic + 'static>,
    ) {
        for diagnostic in diagnostics {
            self.add(module_id, diagnostic);
        }
    }

    /// Check if there are any diagnostics
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get the number of diagnostics
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if there are any error-level diagnostics
    pub fn has_errors(&self) -> bool {
        self.entries
            .iter()
            .any(|entry| entry.diagnostic.severity() == Severity::Error)
    }

    /// Render all diagnostics to a string with colors
    pub fn render(&self, db: &dyn WorkspaceDatabase) -> String {
        self.render_with_config(db, None)
    }

    /// Render all diagnostics to a string without colors
    pub fn render_no_color(&self, db: &dyn WorkspaceDatabase) -> String {
        self.render_with_config(db, Some(Config::default().with_color(false)))
    }

    /// Print all diagnostics to stderr with colors
    pub fn eprint(&self, db: &dyn WorkspaceDatabase) {
        for entry in &self.entries {
            let source_file = db.get_source(entry.module_id);
            if let SourceFile::Raw(raw_file) = source_file {
                let source_id = raw_file.raw_path(db);
                let source = raw_file.contents(db);

                let builder = DiagnosticBuilder::new(entry.diagnostic.as_ref(), db);
                let report = entry
                    .diagnostic
                    .build_report(builder)
                    .build(source_id.to_string());

                report
                    .eprint((source_id.to_string(), Source::from(source.as_ref())))
                    .unwrap();
            }
        }
    }

    /// Internal helper to render with optional config
    fn render_with_config(&self, db: &dyn WorkspaceDatabase, config: Option<Config>) -> String {
        let mut output = String::new();

        for entry in &self.entries {
            let source_file = db.get_source(entry.module_id);

            // Only render diagnostics for raw source files (skip virtual files)
            if let SourceFile::Raw(raw_file) = source_file {
                let source_id = raw_file.raw_path(db);
                let source = raw_file.contents(db);

                let builder = DiagnosticBuilder::new(entry.diagnostic.as_ref(), db);
                let report = entry
                    .diagnostic
                    .build_report(builder)
                    .build_with_config(source_id.to_string(), config);

                let mut buf = Vec::new();
                report
                    .write(
                        (source_id.to_string(), Source::from(source.as_ref())),
                        &mut buf,
                    )
                    .unwrap();

                output.push_str(&String::from_utf8(buf).unwrap());
                output.push('\n');
            }
        }

        output
    }
}
