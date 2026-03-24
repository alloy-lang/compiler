# Plan: Unary Operator Constraint Generation

## Overview

One remaining TODO for generating type constraints on unary operators. Binary operator constraints (arithmetic and custom operators) have been implemented.

---

## TODO: Unary operator constraints

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/constraint_gen/expr/mod.rs` — `infer_unary()`

### Problem

`infer_unary()` ignores the operator and returns the inner expression's type unchanged. Currently only `Neg` exists as a unary operator.

### Current behavior

```rust
fn infer_unary(ctx, source_fql, inner) -> MonoType {
    // TODO: check operator and generate appropriate constraints
    let inner_ty = infer_expr_hm(ctx, inner);
    ctx.assign_type(source_fql, inner_ty)
}
```

### Plan

1. **Extract the operator** from the resolved `Unary { op, expression }`.

2. **Match on operator kind:**

   | Operator | Operand constraint | Result type |
   |----------|--------------------|-------------|
   | `Neg` | Operand must be numeric (`Int` or `Fraction`) | Same as operand type |

3. **Numeric constraint strategy** — Two approaches:

   **Option A — Fresh variable (current-compatible):**
   Constrain the operand via a shared fresh variable. Catches type mismatches with non-numeric types when used with a concrete type.

   **Option B — Introduce a numeric constraint mechanism:**
   Add a `Numeric(var)` constraint to the solver. During solving, verify the variable resolves to `Int` or `Fraction`. This requires the trait constraint infrastructure from `PLAN_generic_type_handling.md`.

   **Recommendation:** Option B once the constraint store is available. Option A as a stopgap.

### Tests

- `-1` — Int, result Int.
- `-1.0` — Fraction, result Fraction.
- `-"hello"` — error, String is not numeric.
- `-x` where `x: Int` — should work.

### Dependencies

- For Option B: depends on the constraint store from `PLAN_generic_type_handling.md`.
