# Plan: Error Reporting

## Overview

Two remaining TODOs related to error reporting: duplicate error deduplication and warning kinds.

Unknown reference error reporting has been implemented — `unknown_reference()` in `HMInferenceContext` now collects `HirResolutionError`s which are converted to diagnostics in the inference output.

---

## TODO 1: Duplicate error reporting (ConflictingTypeAnnotation + UnificationError)

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs` (inference output) and `compiler/hir/hir_ty/src/validation/type_annotation.rs` (validation layer)

### Problem

When a type annotation conflicts with the inferred type, the system generates two errors for the same issue:

1. **`UnificationError`** — emitted during constraint solving when the annotation equation fails to unify (from `alloy_hir_infer`).
2. **`ConflictingTypeAnnotation`** — emitted during the post-inference annotation validation (from `alloy_hir_typed` validation layer).

Both errors point to the same source range and describe the same underlying problem.

### Root cause

In `infer_definition_constraints()`, non-polymorphic annotations are added as type equations (`ctx.add_equation(inferred, annotated, fql)`), producing `UnificationError` on failure. Then `validate_type_annotations()` in the `alloy_hir_typed` layer also checks annotations against inferred types, producing `ConflictingTypeAnnotation` on failure.

### Plan — Three-phase approach

**Phase 1 (Short-term): Range-based deduplication**

After collecting all errors in `type_check_module()`, filter out inference `UnificationError`s that share a `TextRange` with a `ConflictingTypeAnnotation`:

```rust
fn deduplicate_errors(errors: &mut Vec<TypeCheckingError>) {
    let annotation_ranges: FxHashSet<TextRange> = errors.iter()
        .filter(|e| matches!(e.kind, TypeCheckingErrorKind::ConflictingTypeAnnotation { .. }))
        .map(|e| e.range)
        .collect();

    errors.retain(|e| {
        if matches!(e.kind, TypeCheckingErrorKind::InferenceError(_)) {
            !annotation_ranges.contains(&e.range)
        } else {
            true
        }
    });
}
```

**Phase 2 (Medium-term): Prevent the duplicate at source**

Instead of adding the annotation as a unification equation, only use it during the post-unification check. Remove the `add_equation` call for annotations and rely solely on `validate_type_annotations()`.

**Phase 3 (Long-term): Error recovery with tracking**

Maintain a set of expression FQLs that already have errors. Before adding an error for an expression, check if it's already errored. This prevents all cascading errors.

### Recommendation

Start with **Phase 1** — it's safe, isolated, and immediately improves the user experience.

### Tests

- Update the `conflicting_type_annotation` test in `compiler/hir/hir_ty/src/lib.rs` to expect exactly one error.
- Ensure standalone `UnificationError`s (not caused by annotations) are preserved.

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

---

## Implementation Order

1. **TODO 1, Phase 1** — Range-based deduplication. Standalone change.
2. **TODO 2** — Define at least one warning variant.
3. **TODO 1, Phase 2/3** — Deeper deduplication work. Can be deferred.
