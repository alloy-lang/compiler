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

    /// Returns true if this diagnostic should be hidden given the presence of `other`.
    ///
    /// Used for error precedence: higher-priority diagnostics at overlapping ranges
    /// suppress lower-priority ones (e.g., a specific type annotation error suppresses
    /// a generic unification error for the same range).
    fn is_hidden_by(&self, _other: &dyn Diagnostic) -> bool {
        false
    }

    /// Check whether two text ranges overlap.
    ///
    /// Semantics:
    /// - Non-empty ranges overlap only when they have positive-width intersection.
    /// - A point range (`start == end`) overlaps a non-empty range only if the
    ///   point lies strictly inside the non-empty range.
    /// - Two point ranges overlap only when they are at the same offset.
    fn overlaps_with(&self, other: &dyn Diagnostic) -> bool {
        let lhs = self.primary_span();
        let rhs = other.primary_span();

        let lhs_empty = lhs.start() == lhs.end();
        let rhs_empty = rhs.start() == rhs.end();

        match (lhs_empty, rhs_empty) {
            (false, false) => lhs.start() < rhs.end() && rhs.start() < lhs.end(),
            (true, true) => lhs.start() == rhs.start(),
            (true, false) => rhs.start() <= lhs.start() && lhs.start() < rhs.end(),
            (false, true) => lhs.start() <= rhs.start() && rhs.start() < lhs.end(),
        }
    }

    /// Check overlap, but treat `other` point-spans as inclusive on this
    /// diagnostic's end boundary.
    ///
    /// Useful for recovery diagnostics where a missing-token point can be
    /// emitted just after the token that semantically triggered the error.
    fn overlaps_or_contains(&self, other: &dyn Diagnostic) -> bool {
        let self_span = self.primary_span();
        let other_span = other.primary_span();

        if other_span.start() == other_span.end() {
            let point = other_span.start();
            self_span.start() <= point && point <= self_span.end()
        } else {
            self.overlaps_with(other)
        }
    }

    /// Upcast to `Any` for downcasting to concrete types within the same crate.
    fn as_any(&self) -> &dyn std::any::Any;
}

#[cfg(test)]
mod tests {
    use super::{Diagnostic, Severity};
    use text_size::TextRange;

    #[derive(Debug)]
    struct DummyDiagnostic {
        span: TextRange,
    }

    impl Diagnostic for DummyDiagnostic {
        fn severity(&self) -> Severity {
            Severity::Error
        }

        fn message(&self) -> String {
            "dummy diagnostic".to_string()
        }

        fn primary_span(&self) -> TextRange {
            self.span
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
    }

    #[test]
    fn overlaps_when_ranges_intersect() {
        let lhs = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let rhs = DummyDiagnostic {
            span: TextRange::new(3.into(), 8.into()),
        };

        assert!(lhs.overlaps_with(&rhs));
        assert!(rhs.overlaps_with(&lhs));
    }

    #[test]
    fn does_not_overlap_when_ranges_are_disjoint() {
        let lhs = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let rhs = DummyDiagnostic {
            span: TextRange::new(6.into(), 8.into()),
        };

        assert!(!lhs.overlaps_with(&rhs));
        assert!(!rhs.overlaps_with(&lhs));
    }

    #[test]
    fn does_not_overlap_when_ranges_only_touch_at_boundary() {
        let lhs = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let rhs = DummyDiagnostic {
            span: TextRange::new(5.into(), 8.into()),
        };

        assert!(!lhs.overlaps_with(&rhs));
        assert!(!rhs.overlaps_with(&lhs));
    }

    #[test]
    fn does_not_overlap_when_point_span_is_at_range_end() {
        let range = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let point_at_end = DummyDiagnostic {
            span: TextRange::new(5.into(), 5.into()),
        };

        assert!(!range.overlaps_with(&point_at_end));
        assert!(!point_at_end.overlaps_with(&range));
    }

    #[test]
    fn overlaps_when_point_span_is_inside_range() {
        let range = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let point_inside = DummyDiagnostic {
            span: TextRange::new(3.into(), 3.into()),
        };

        assert!(range.overlaps_with(&point_inside));
        assert!(point_inside.overlaps_with(&range));
    }

    #[test]
    fn overlaps_or_contains_other_point_includes_end_boundary_point() {
        let range = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let point_at_end = DummyDiagnostic {
            span: TextRange::new(5.into(), 5.into()),
        };

        assert!(range.overlaps_or_contains(&point_at_end));
    }

    #[test]
    fn overlaps_or_contains_other_point_delegates_for_non_point_ranges() {
        let lhs = DummyDiagnostic {
            span: TextRange::new(0.into(), 5.into()),
        };
        let rhs = DummyDiagnostic {
            span: TextRange::new(5.into(), 8.into()),
        };

        assert!(!lhs.overlaps_or_contains(&rhs));
    }
}
