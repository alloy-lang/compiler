# Plan: Error Reporting

## Overview

Three TODOs related to error reporting: duplicate error deduplication, an unimplemented warning kind, and missing error reporting for unknown references.

---

## TODO 1: Duplicate error reporting (ConflictingTypeAnnotation + UnificationError)

**File:** `src/diagnostics.rs:228`

### Problem

When a type annotation conflicts with the inferred type, the system generates two errors for the same issue:

1. **`UnificationError`** (E002) — emitted during constraint solving when the annotation equation fails to unify.
2. **`ConflictingTypeAnnotation`** (E001) — emitted during the post-unification annotation check.

Both errors point to the same source range and describe the same underlying problem.

### Root cause

In `hm/inference.rs`, the annotation is added as both:
- A unification equation (`ctx.add_equation(inferred, annotated, fql)`) — produces `UnificationError` on failure.
- A post-unification compatibility check (`check_type_annotation()`) — produces `ConflictingTypeAnnotation` on failure.

### Plan — Three-phase approach

**Phase 1 (Short-term): Range-based deduplication**

After collecting all errors, filter out `UnificationError`s that share a `TextRange` with a `ConflictingTypeAnnotation`:

```rust
fn deduplicate_errors(errors: &mut Vec<TypeInferenceError>) {
    let annotation_ranges: FxHashSet<TextRange> = errors.iter()
        .filter(|e| matches!(e.kind, TypeInferenceErrorKind::ConflictingTypeAnnotation { .. }))
        .map(|e| e.range)
        .collect();

    errors.retain(|e| {
        if matches!(e.kind, TypeInferenceErrorKind::UnificationError(_)) {
            !annotation_ranges.contains(&e.range)
        } else {
            true
        }
    });
}
```

Call this at the end of `infer_types_hm()` before returning the `HirTypedModule`.

**Phase 2 (Medium-term): Prevent the duplicate at source**

Instead of adding the annotation as a unification equation, only use it during the post-unification check. Remove the `add_equation` call for annotations and rely solely on `check_type_annotation()`:

- In `hm/inference.rs`, where annotation equations are added, skip the equation and let the annotation check handle the error.
- This requires that the annotation still constrains inference — possibly by using the annotation as a type hint during generalization rather than a hard equation.

**Phase 3 (Long-term): Error recovery with tracking**

Maintain a set of expression FQLs that already have errors. When generating new errors, skip expressions in this set:

```rust
errored_expressions: FxHashSet<EPFql>
```

Before adding an error for an expression, check if it's already errored. This prevents all cascading errors, not just the annotation/unification duplicate.

### Recommendation

Start with **Phase 1** — it's safe, isolated, and immediately improves the user experience. Phase 2 can follow once the interaction between annotation equations and inference is better understood. Phase 3 is a broader refactor that benefits all error paths.

### Tests

- Update the existing `conflicting_type_annotation` test (lib.rs:289) to expect exactly one error instead of two.
- Ensure that standalone `UnificationError`s (not caused by annotations) are preserved.

---

## TODO 2: Bare TODO in TypeInferenceWarningKind

**File:** `src/diagnostics.rs:274`

### Problem

`TypeInferenceWarningKind` is an empty enum with just a `// TODO` comment. No warnings are ever emitted.

### Plan

Define warning kinds for common non-error situations:

```rust
pub enum TypeInferenceWarningKind {
    UnusedVariable { name: String },
    RedundantTypeAnnotation,
    UnreachablePattern,
    // Add more as the type checker matures
}
```

Initially, implement at least one warning to validate the pipeline:

1. **`UnusedVariable`** — detect variables bound in patterns but never referenced. This requires tracking variable usage during constraint generation.
2. Wire it into `HirTypedModule::warnings()` which already exists but returns an empty vec.
3. Decide on the test policy (see infrastructure plan for the `fail_on_warnings` decision).

### Implementation steps

1. Add one or more variants to `TypeInferenceWarningKind`.
2. Add warning emission points in the appropriate phases.
3. Ensure `HirTypedModule::warnings()` returns them.
4. Add test cases that trigger each warning.

---

## TODO 3: Report error for unknown references

**File:** `src/hir_ty/hm/mod.rs:321`

### Problem

`unknown_reference()` silently creates a fresh type variable when a name cannot be resolved. No error is reported, so the user gets confusing downstream unification errors instead of a clear "undefined variable" message.

### Current behavior

```rust
pub(super) fn unknown_reference(&mut self, fql: impl Into<EPFql>) -> MonoType {
    // TODO: report an error when we can't find a reference by name
    let ty = self.fresh_type_var();
    self.assign_type(fql.into(), ty)
}
```

### Plan

1. Add an error report in `unknown_reference()`:
   ```rust
   pub(super) fn unknown_reference(&mut self, fql: impl Into<EPFql>) -> MonoType {
       let fql = fql.into();
       self.report_resolution_error(TypeResolutionError::UnknownReference(fql.clone()));
       let ty = self.fresh_type_var();
       self.assign_type(fql, ty)
   }
   ```

2. Still return a fresh type variable so inference can continue (error recovery). This prevents cascading errors from the unresolved reference.

3. Consider whether to also use `ResolvedType::Error` (or similar sentinel) to suppress downstream errors caused by the unresolved reference. This ties into the Phase 3 error recovery strategy from TODO 1.

### Tests

- Reference to an undefined variable — should produce a clear `TypeResolutionError`.
- The error should not cause cascading unification errors.

---

## Implementation Order

1. **TODO 3** — Quick fix: add error reporting to `unknown_reference()`. Standalone change.
2. **TODO 1, Phase 1** — Range-based deduplication. Standalone change.
3. **TODO 2** — Define at least one warning variant. Depends on deciding the test policy.
4. **TODO 1, Phase 2/3** — Deeper deduplication work. Can be deferred.
