# Plan: Error Reporting

## Overview

Two TODOs related to error reporting. Error deduplication is implemented; warning kinds remain.

---

## TODO 1: Duplicate error reporting — DONE

Implemented via `is_hidden_by` on the `Diagnostic` trait. Each diagnostic declares what suppresses it using `as_any()` downcasting for intra-crate type matching and cross-crate `ParseError` checks. `DiagnosticsReporter::filter_hidden()` applies O(n²) filtering before rendering.

**Suppression rules:**
- E33001 (TypeMismatch, wrapped) hidden by E34001 or E34003 at overlapping range
- E34001 (ConflictingTypeAnnotation) hidden by E34003 at overlapping range
- Wrapped E33xxx/E32xxx hidden by ParseError at overlapping range

**Key files:**
- `compiler/diagnostics/src/core.rs` — `is_hidden_by`, `overlaps_with`, `as_any` on `Diagnostic` trait
- `compiler/diagnostics/src/reporter.rs` — `filter_hidden()` method
- `compiler/hir/hir_infer/src/diagnostics.rs` — `TypeInferenceError::is_hidden_by`
- `compiler/hir/hir_ty/src/diagnostics.rs` — `TypeCheckingError::is_hidden_by`

---

## TODO 2: Warning kinds

**File:** `compiler/hir/hir_infer/src/diagnostics.rs`

### Problem

`TypeInferenceWarningKind` is an empty enum with no variants. No warnings are ever emitted.

### Plan

Define warning kinds for common non-error situations:

```rust
pub enum TypeInferenceWarningKind {
    UnusedVariable { name: String },
    RedundantTypeAnnotation,
    UnreachablePattern,
}
```

Initially, implement at least one warning to validate the pipeline:

1. **`UnusedVariable`** — detect variables bound in patterns but never referenced.
2. Wire it through `HirInferredModule::warnings()` and `HirTypedModule::warnings()`.

### Dependencies

- Decide on test failure policy for warnings (see `PLAN_infrastructure.md`).
