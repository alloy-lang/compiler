//! Core diagnostic types and traits

use ariadne::{Color, ReportKind};
use text_size::TextRange;

use crate::builder::DiagnosticBuilder;

/// The severity level of a diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// A note or informational message
    Note,
    /// A warning that doesn't prevent compilation
    Warning,
    /// An error that prevents compilation
    Error,
}

impl Severity {
    /// Convert to ariadne's ReportKind
    pub fn to_report_kind(&self) -> ReportKind<'static> {
        match self {
            Severity::Note => ReportKind::Advice,
            Severity::Warning => ReportKind::Warning,
            Severity::Error => ReportKind::Error,
        }
    }

    /// Get the default color for this severity
    pub fn color(&self) -> Color {
        match self {
            Severity::Note => Color::Cyan,
            Severity::Warning => Color::Yellow,
            Severity::Error => Color::Red,
        }
    }
}

/// A label to attach to a source span
#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    /// The span this label refers to
    pub range: TextRange,
    /// The message for this label
    pub message: String,
    /// The color of the label (defaults to the severity color)
    pub color: Option<Color>,
}

impl DiagnosticLabel {
    /// Create a new label
    pub fn new(range: TextRange, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
            color: None,
        }
    }

    /// Set the color of this label
    pub fn with_color(mut self, color: Color) -> Self {
        self.color = Some(color);
        self
    }
}

/// The main trait that all diagnostics must implement
pub trait Diagnostic: std::fmt::Debug {
    /// The severity of this diagnostic
    fn severity(&self) -> Severity;

    /// An optional error code (e.g., "E001")
    fn code(&self) -> Option<&str> {
        None
    }

    /// The main error message
    fn message(&self) -> String;

    /// The primary source span for this diagnostic
    fn primary_span(&self) -> TextRange;

    /// Build the full diagnostic report
    ///
    /// Override this method to add labels, notes, and help text
    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        builder.with_primary_label(self.message())
    }
}
