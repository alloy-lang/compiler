//! Compiler diagnostic system
//!
//! This crate provides a unified interface for reporting errors, warnings, and other diagnostics
//! across all compiler modules. It uses the ariadne library for beautiful error rendering.
//!
//! # Design Philosophy
//!
//! - **Trait-based**: Errors implement the `Diagnostic` trait
//! - **Extensible**: Easy to add new error types from any module
//! - **Consistent**: All errors render in a similar style
//! - **Rich**: Support for labels, notes, help text, and error codes
//!
//! # Example
//!
//! ```ignore
//! use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, Severity};
//! use text_size::TextRange;
//!
//! #[derive(Debug)]
//! struct MyError {
//!     range: TextRange,
//!     message: String,
//! }
//!
//! impl Diagnostic for MyError {
//!     fn severity(&self) -> Severity {
//!         Severity::Error
//!     }
//!
//!     fn code(&self) -> Option<&str> {
//!         Some("E001")
//!     }
//!
//!     fn message(&self) -> String {
//!         self.message.clone()
//!     }
//!
//!     fn primary_span(&self) -> TextRange {
//!         self.range
//!     }
//!
//!     fn build_report(&self, builder: DiagnosticBuilder) -> DiagnosticBuilder {
//!         builder
//!             .with_primary_label("error occurred here")
//!             .with_help("Try fixing this by...")
//!     }
//! }
//! ```

mod builder;
mod core;
mod reporter;

// Re-export public API
pub use builder::DiagnosticBuilder;
pub use core::{Diagnostic, DiagnosticLabel, Severity};
pub use reporter::DiagnosticsReporter;

#[cfg(test)]
mod test_db;

#[cfg(test)]
mod tests {
    use super::*;
    use ariadne::Source;
    use test_db::TestDiagnosticsDatabase;
    use text_size::TextRange;

    #[derive(Debug)]
    struct TestError {
        range: TextRange,
        message: String,
    }

    impl Diagnostic for TestError {
        fn severity(&self) -> Severity {
            Severity::Error
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn code(&self) -> Option<&str> {
            Some("TEST001")
        }

        fn message(&self) -> String {
            self.message.clone()
        }

        fn primary_span(&self) -> TextRange {
            self.range
        }

        fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
            builder
                .with_primary_label("error occurred here")
                .with_help("This is how you fix it")
        }
    }

    fn render_diagnostic(
        diagnostic: &dyn Diagnostic,
        source_id: impl Into<String>,
        source: &str,
    ) -> String {
        let db = TestDiagnosticsDatabase::default();

        let source_id = source_id.into();
        let builder = DiagnosticBuilder::new(diagnostic, &db);
        let report = diagnostic.build_report(builder).build(source_id.clone());

        let mut buf = Vec::new();
        report
            .write((source_id, Source::from(source)), &mut buf)
            .unwrap();
        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn test_diagnostic_render() {
        let error = TestError {
            range: TextRange::new(5.into(), 10.into()),
            message: "Test error message".to_string(),
        };

        let source = "let x = 123;";
        let rendered = render_diagnostic(&error, "test.alloy", source);

        assert!(rendered.contains("Test error message"));
        assert!(rendered.contains("TEST001"));
    }
}
