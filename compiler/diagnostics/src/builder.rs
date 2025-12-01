//! Diagnostic builder for constructing rich error reports

use ariadne::{Config, Label, Report};
use std::ops::Range;

use crate::core::{Diagnostic, DiagnosticLabel};

/// Builder for constructing rich diagnostics
#[derive(Debug)]
pub struct DiagnosticBuilder<'a> {
    diagnostic: &'a dyn Diagnostic,
    labels: Vec<DiagnosticLabel>,
    notes: Vec<String>,
    help: Option<String>,
}

impl<'a> DiagnosticBuilder<'a> {
    /// Create a new builder for a diagnostic
    pub fn new(diagnostic: &'a dyn Diagnostic) -> Self {
        Self {
            diagnostic,
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }

    /// Add a label to the primary span
    pub fn with_primary_label(mut self, message: impl Into<String>) -> Self {
        let range = self.diagnostic.primary_span();
        self.labels.push(DiagnosticLabel::new(range, message));
        self
    }

    /// Add a secondary label to another span
    pub fn with_label(mut self, label: DiagnosticLabel) -> Self {
        self.labels.push(label);
        self
    }

    /// Add a note
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Set the help message
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Build the ariadne Report
    pub fn build(self, source_id: String) -> Report<'static, (String, Range<usize>)> {
        self.build_with_config(source_id, None)
    }

    /// Build the ariadne Report with a custom config
    pub fn build_with_config(
        self,
        source_id: String,
        config: Option<Config>,
    ) -> Report<'static, (String, Range<usize>)> {
        let primary_span = self.diagnostic.primary_span();
        let span_range = usize::from(primary_span.start())..usize::from(primary_span.end());

        let mut report_builder = Report::build(
            self.diagnostic.severity().to_report_kind(),
            source_id.clone(),
            span_range.start,
        );

        // Apply config if provided
        if let Some(config) = config {
            report_builder = report_builder.with_config(config);
        }

        let mut report = report_builder.with_message(self.diagnostic.message());

        // Add error code if present
        if let Some(code) = self.diagnostic.code() {
            report = report.with_code(code);
        }

        // Add labels
        for label in self.labels {
            let label_range =
                usize::from(label.range.start())..usize::from(label.range.end());
            let mut ariadne_label = Label::new((source_id.clone(), label_range))
                .with_message(label.message);

            if let Some(color) = label.color {
                ariadne_label = ariadne_label.with_color(color);
            } else {
                ariadne_label =
                    ariadne_label.with_color(self.diagnostic.severity().color());
            }

            report = report.with_label(ariadne_label);
        }

        // Add notes
        for note in self.notes {
            report = report.with_note(note);
        }

        // Add help
        if let Some(help) = self.help {
            report = report.with_help(help);
        }

        report.finish()
    }
}
