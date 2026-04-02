# Plan: Pattern & Constructor Types

## Overview

Two TODOs related to type-checking patterns: looking up constructor type schemes for destructure patterns, and clarifying how pattern types interact with expression-level type annotations.

---

## TODO 1: Look up constructor type scheme from target and scope ✅ DONE

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/constraint_gen/pattern.rs` — `infer_destructure()`

### Implementation

`infer_destructure()` now:
1. Resolves the type definition and finds the correct member (single or named variant)
2. Reuses `build_constructor_type()` from the expression side to build the curried constructor type
3. Generalizes and instantiates to get fresh type vars per pattern occurrence (prevents sharing between multiple destructure patterns in the same scope)
4. Decomposes the function type into field types and result type
5. Constrains each pattern arg against the corresponding field type

### Tests

- `match_variant_destructure_constrains_field_type` — union variant field → Int
- `match_data_destructure_constrains_field_type` — single variant field → Int
- `match_destructure_propagates_type_across_arms` — arm agreement constrains both variants
- Stdlib `option.alloy` and `either.alloy` now pass with correct types

---

## TODO 2: Pattern type annotations from expressions ✅ DONE (no changes needed)

**Conclusion:** Pattern types are already correctly constrained. No explicit pattern-level validation is needed.

- **Monomorphic annotations**: Added as equations in `infer_body_type`. Unification propagates annotation arg types to pattern variables (e.g., `typeof x : String -> String` makes pattern `s` in `|s| -> ""` resolve to `String`).
- **Polymorphic annotations**: Skipped at inference level (`is_polymorphic()` guard). Patterns get types from body inference. `validate_type_annotations` compares the full expression type (which includes pattern-derived arg types) against the annotation — mismatches are caught at the function type level.

---

## Implementation Order

1. **TODO 1** — Implement constructor type lookup in `infer_destructure`. Blocks correct type inference for all pattern matching on algebraic data types.
2. **TODO 2** — Audit and clarify the pattern annotation check. Lower priority cleanup task.
