# Migration Guide: Using the Diagnostic Trait

This guide shows how to integrate the `Diagnostic` trait into your compiler modules.

## Overview

The `alloy_diagnostics` crate provides a unified interface for reporting errors, warnings, and other diagnostics. All error types should implement the `Diagnostic` trait to enable consistent, beautiful error reporting.

## Basic Usage

### 1. Add dependency

In your module's `Cargo.toml`:

```toml
[dependencies]
alloy_diagnostics = { path = "../diagnostics" }
text-size = { workspace = true }
```

### 2. Implement the Diagnostic trait

```rust
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticExt, Severity};
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeMismatchError {
    pub range: TextRange,
    pub expected: String,
    pub found: String,
}

impl Diagnostic for TypeMismatchError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        Some("E001")
    }

    fn message(&self) -> String {
        format!("Type mismatch: expected {}, found {}", self.expected, self.found)
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        builder
            .with_primary_label(format!("expected `{}`, found `{}`", self.expected, self.found))
            .with_help(format!("Consider converting `{}` to `{}`", self.found, self.expected))
    }
}
```

### 3. Use the diagnostic

```rust
let error = TypeMismatchError {
    range: TextRange::new(10.into(), 20.into()),
    expected: "Int".to_string(),
    found: "String".to_string(),
};

// Render to string
let output = error.render("main.alloy", source_code);
println!("{}", output);

// Or print directly to stderr (colored output)
error.eprint("main.alloy", source_code);
```

## Migration Example: hir_ty Module

Here's how to migrate the existing `TypeInferenceError` from `hir_ty`:

### Before (current implementation)

```rust
// In hir_ty/src/diagnostics.rs
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceError {
    kind: TypeInferenceErrorKind,
    range: TextRange,
}

impl TypeInferenceError {
    pub fn render(&self, source_id: impl Into<String>, source: &str) -> String {
        // Custom ariadne rendering code...
    }
}
```

### After (using Diagnostic trait)

```rust
// In hir_ty/src/diagnostics.rs
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, DiagnosticExt, DiagnosticLabel, Severity};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceError {
    kind: TypeInferenceErrorKind,
    range: TextRange,
}

impl Diagnostic for TypeInferenceError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { .. } => Some("E001"),
            TypeInferenceErrorKind::UnificationError(_) => Some("E002"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { .. } => {
                "Type annotation conflict".to_string()
            }
            TypeInferenceErrorKind::UnificationError(_) => {
                "Type mismatch".to_string()
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { expected, found } => {
                builder
                    .with_primary_label(format!("expected `{}`, found `{}`", expected, found))
                    .with_help(format!(
                        "The type annotation says this should be `{}`, but type inference determined it to be `{}`",
                        expected, found
                    ))
            }
            TypeInferenceErrorKind::UnificationError(unif_err) => {
                // Handle unification errors...
                builder.with_primary_label("types don't match")
            }
        }
    }
}
```

## Advanced Features

### Multiple Labels

You can add labels to multiple spans:

```rust
fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
    builder
        .with_primary_label("this has type `Int`")
        .with_label(DiagnosticLabel::new(
            self.other_range,
            "but this has type `String`"
        ))
        .with_help("These types must match")
}
```

### Notes

Add additional context with notes:

```rust
fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
    builder
        .with_primary_label("error here")
        .with_note("This is caused by the type system constraint")
        .with_note("Related: issue #42")
}
```

### Colored Labels

Customize label colors:

```rust
use ariadne::Color;

fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
    builder
        .with_label(
            DiagnosticLabel::new(self.range, "important")
                .with_color(Color::Magenta)
        )
}
```

## Working with Trait Objects

For collections of different error types, use trait objects:

```rust
let errors: Vec<Box<dyn Diagnostic>> = vec![
    Box::new(type_error),
    Box::new(parse_error),
    Box::new(lowering_error),
];

// Render all errors
for error in &errors {
    error.render("main.alloy", source_code);
}
```

## Best Practices

1. **Use descriptive error codes**: E001, E002, etc.
2. **Keep messages concise**: The primary message should be a short summary
3. **Use labels for details**: Add specific information in labels
4. **Provide help text**: Guide users toward solutions
5. **Be consistent**: Follow the same patterns across your module

## Common Patterns

### Wrapper for External Errors

If you're wrapping errors from other libraries:

```rust
#[derive(Debug)]
pub struct ParseError {
    range: TextRange,
    inner: rowan::Error,
}

impl Diagnostic for ParseError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self) -> String {
        format!("Parse error: {}", self.inner)
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }
}
```

### Enum of Error Types

For modules with multiple error kinds:

```rust
#[derive(Debug)]
pub enum ModuleError {
    Type(TypeInferenceError),
    Parse(ParseError),
    Resolution(ResolutionError),
}

impl Diagnostic for ModuleError {
    fn severity(&self) -> Severity {
        match self {
            ModuleError::Type(e) => e.severity(),
            ModuleError::Parse(e) => e.severity(),
            ModuleError::Resolution(e) => e.severity(),
        }
    }

    fn code(&self) -> Option<&str> {
        match self {
            ModuleError::Type(e) => e.code(),
            ModuleError::Parse(e) => e.code(),
            ModuleError::Resolution(e) => e.code(),
        }
    }

    fn message(&self) -> String {
        match self {
            ModuleError::Type(e) => e.message(),
            ModuleError::Parse(e) => e.message(),
            ModuleError::Resolution(e) => e.message(),
        }
    }

    fn primary_span(&self) -> TextRange {
        match self {
            ModuleError::Type(e) => e.primary_span(),
            ModuleError::Parse(e) => e.primary_span(),
            ModuleError::Resolution(e) => e.primary_span(),
        }
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match self {
            ModuleError::Type(e) => e.build_report(builder),
            ModuleError::Parse(e) => e.build_report(builder),
            ModuleError::Resolution(e) => e.build_report(builder),
        }
    }
}
```

## Testing

Test your diagnostic output:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_message() {
        let error = TypeMismatchError { /* ... */ };
        let output = error.render("test.alloy", "let x = 1");

        assert!(output.contains("Type mismatch"));
        assert!(output.contains("E001"));
    }
}
```
