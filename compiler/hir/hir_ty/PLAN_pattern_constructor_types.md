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

## TODO 2: Pattern type annotations from expressions

**File:** `compiler/hir/hir_ty/src/validation/type_annotation.rs`

### Problem

Patterns cannot have type annotations directly in the syntax, but their types can be specified by annotations on the enclosing expression. The interaction between expression annotations and pattern types needs clarification.

### Plan

1. **Audit** whether pattern types are already constrained through the shared HM context (expression annotations propagate to patterns via unification).
2. **If redundant**: document that pattern types are constrained through expression annotations via unification, no explicit check needed.
3. **If needed**: ensure the validation layer handles the indirection correctly.

---

## Implementation Order

1. **TODO 1** — Implement constructor type lookup in `infer_destructure`. Blocks correct type inference for all pattern matching on algebraic data types.
2. **TODO 2** — Audit and clarify the pattern annotation check. Lower priority cleanup task.
