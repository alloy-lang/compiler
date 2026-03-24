# Plan: Per-Definition Salsa Inference

## Overview

Refactor type inference from a monolithic per-module pass with a shared `HMInferenceContext` into per-definition Salsa-tracked queries. This is the standard architecture for incremental type inference (used by rust-analyzer, GHC, etc.).

---

## Current Architecture

`infer_types_module(module_id)` calls `infer_types_hm()`, which:

1. Creates a **single shared `HMInferenceContext`** for the entire module
2. Phase 1: Iterates all definitions, generates all constraints into one pool
3. Phase 2: Solves all equations at once
4. Phase 3: Generalizes polymorphic definitions
5. Phase 4: Applies substitution and converts to output types

**Per-definition queries exist but are underused:**
- `infer_value_signature(value_def)` — Salsa tracked, returns a definition's type
- `infer_body_type(value_def)` — Salsa tracked, infers a definition's type in isolation

These are only called as escape hatches when `infer_expr_hm` encounters:
- Cross-module references
- Same-module references to definitions with polymorphic annotations

Same-module unannotated definitions go through the shared context instead.

### Problems

1. **No incremental granularity.** Changing any definition invalidates the entire module's inference.
2. **Two code paths.** Cross-module refs use Salsa queries; same-module refs use the shared context. This split caused the Phase 1/3 poly_env bugs and the monad join bug.
3. **Complexity.** The 4-phase pipeline, `inferring_expr` cycle guard, and pre-solving poly_env entries exist solely to manage the shared context.

---

## Target Architecture

Each definition's inference is its own Salsa-tracked query. The module-level query just aggregates results.

### Core change: expand `infer_body_type`

**Current** (`inference.rs:152`):
```rust
#[salsa::tracked(cycle_initial = infer_body_type_cycle_initial)]
fn infer_body_type(db, value_def: ValueDef) -> InferredType
```

**Target:**
```rust
#[salsa::tracked(cycle_initial = infer_body_type_cycle_initial)]
fn infer_body_type(db, value_def: ValueDef) -> DefinitionInferenceResult

struct DefinitionInferenceResult {
    definition_type: InferredType,
    expression_types: FxHashMap<ExpressionIdx, InferredType>,
    pattern_types: FxHashMap<PatternIdx, InferredType>,
    errors: Vec<TypeInferenceError>,
}
```

Each call creates its own `HMInferenceContext`, generates constraints for the body, solves them, and returns the full sub-expression type map.

### Core change: uniform reference resolution

**Current** (`constraint_gen/expr/mod.rs:34-53`):
```rust
// Two paths depending on cross-module vs same-module, annotated vs not
let is_cross_module = source_fql.module_id != ctx.module_id;
let has_poly_annotation = ...;
if is_cross_module || has_poly_annotation {
    let sig = value::infer(ctx.db, value_def);  // Salsa query
    ...
}
// else: infer through shared context
```

**Target:**
```rust
// One path for all value definition references
if let Some(value_def) = hir::module_value_def(ctx.db, ...) {
    if !is_self {
        let sig = value::infer(ctx.db, value_def);  // Always Salsa
        let mono_ty = inferred_to_mono(&sig, ctx);
        return ctx.generalize_to_poly(mono_ty, &source_fql);
    }
}
```

### Core change: thin module aggregator

**Current** (`inference.rs:30-115`): 85-line function with 4 phases.

**Target:**
```rust
fn infer_types_hm(db, module_id) -> HirInferredModule {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let mut result = HirInferredModule::empty(module_id);

    for (&expr_id, _value) in hir_module.values() {
        let value_def = hir::module_value_def(db, module_id, expr_id);
        let def_result = infer_body_type(db, value_def);
        result.merge(def_result);
    }

    // Handle bare top-level expressions (not bound to a value def)
    for (expr_id, ..) in hir_module.expressions() {
        if hir_module.get_value_by_id(&expr_id).is_some() {
            continue;
        }
        let bare_result = infer_bare_expression(db, module_id, expr_id);
        result.merge_bare(bare_result);
    }

    result
}
```

---

## What Gets Removed

- **The shared `HMInferenceContext` in `infer_types_hm`** — each definition gets its own context
- **The 4-phase pipeline** — each definition does generate + solve + convert independently
- **`inferring_expr`** — no longer needed; Salsa's `cycle_initial` handles cycles
- **Phase 1 immediate generalization** — no pre-solving poly_env entries from annotations
- **Phase 3 post-solving generalization** — definitions generalize in their own `infer_body_type`
- **The `is_cross_module || has_poly_annotation` branching** — all refs use `infer_value_signature`

---

## Behavioral Change: Call-Site Refinement

The shared context currently lets call-site constraints refine unannotated definitions:

```
let x = |a, b| -> a + b   -- currently inferred as Int -> Int -> Int
let y = x(1, 2)            -- because y's call constrains x
```

With per-definition inference, `x` gets its principal type `t -> t -> t` from its body alone. `y` still gets `Int` from instantiation.

**Why this is OK:**
- Standard HM behavior — a definition's type shouldn't depend on how it's called
- Adding a new call site currently changes existing definitions' types (surprising)
- The language already requires `typeof` annotations for polymorphic definitions
- `infer_lambda_based_on_usage` test would change: `x` stays generic, `y` stays `Int`

---

## Implementation Steps

### Step 1: Define `DefinitionInferenceResult`

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs`

Create the result struct that `infer_body_type` will return:

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionInferenceResult {
    pub definition_type: InferredType,
    pub expression_types: FxHashMap<ExpressionIdx, InferredType>,
    pub pattern_types: FxHashMap<PatternIdx, InferredType>,
    pub errors: Vec<TypeInferenceError>,
}
```

### Step 2: Expand `infer_body_type` to collect sub-expression types

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs`

Currently `infer_body_type` creates a context, infers, solves, and returns just the top-level type. Expand it to also iterate `ctx.type_env` and `ctx.poly_env` after solving, converting all entries to `InferredType` and populating `expression_types` / `pattern_types`.

This is essentially extracting the Phase 4 logic from `infer_types_hm` into `infer_body_type`.

### Step 3: Remove the `is_cross_module || has_poly_annotation` guard

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/constraint_gen/expr/mod.rs`

Change `infer_expr_hm` to use `infer_value_signature` for ALL value definition references (not just cross-module/annotated ones). The only exception remains the definition currently being inferred (`is_self` check for cycle prevention).

```rust
if !is_self {
    if let Some(value_def) = hir::module_value_def(ctx.db, source_fql.module_id, source_fql.local_id) {
        let sig = value::infer(ctx.db, value_def);
        let mono_ty = inferred_to_mono(&sig, ctx);
        return ctx.generalize_to_poly(mono_ty, &source_fql);
    }
}
```

### Step 4: Simplify `infer_types_hm` to aggregate per-definition results

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs`

Replace the 4-phase pipeline with a loop that calls `infer_body_type` for each definition and merges the results. Remove the shared context, `inferring_expr`, Phase 1 poly_env entries, and Phase 3 generalization.

### Step 5: Update tests

- `infer_lambda_based_on_usage`: `x` stays `t -> t -> t` (generic), `y` stays `Int`
- `infer_variable_ref_literal`: `y` still resolves to `Int` (through `infer_value_signature`)
- Other tests should be unaffected — they either have annotations or don't depend on cross-definition refinement

### Step 6: Clean up dead code

- Remove `inferring_expr` from `HMInferenceContext`
- Remove `Phase 1/3` generalization logic
- Simplify `HMInferenceContext::new()` (no longer needs to track module-wide state)
- Consider removing `poly_env` from `HMInferenceContext` if only used within a single definition's scope

---

## Risk: Mutual Recursion

Mutual recursion between unannotated definitions is the main risk area. Example:

```
let f = |x| -> g(x)
let g = |y| -> f(y)
```

With per-definition inference:
- `infer_body_type(f)` calls `infer_value_signature(g)` which calls `infer_body_type(g)`
- `infer_body_type(g)` calls `infer_value_signature(f)` — **cycle detected**
- Salsa returns `cycle_initial` value (`InferredType::Unconstrained`) for `f`
- `g` resolves with `f: Unconstrained`, gets type `t -> Unconstrained`
- Salsa re-runs the cycle with the new value until convergence

This is correct behavior — Salsa's fixed-point iteration handles it. The existing `cycle_initial` on both `infer_body_type` and `infer_value_signature` already supports this.

For mutual recursion with annotations, the annotation provides the type without needing body inference, so no cycle occurs.

---

## Verification

```bash
# Unit tests
cargo test --color=always --no-fail-fast --lib -p alloy_hir_infer -- --no-capture

# Integration tests (stdlib)
cargo test --color=always --no-fail-fast --lib -p alloy_hir_typed -- --no-capture

# Full workspace
cargo test --color=always --no-fail-fast --workspace -- --no-capture
```
