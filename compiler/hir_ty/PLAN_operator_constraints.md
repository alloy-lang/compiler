# Plan: Operator Constraint Generation

## Overview

Two TODOs in `src/hir_ty/hm/constraint_gen/expr.rs` for generating type constraints on binary and unary operators. Currently both functions ignore the operator entirely.

---

## TODO 1: Binary operator constraints

**File:** `src/hir_ty/hm/constraint_gen/expr.rs:204`

### Problem

`infer_binary()` receives `lhs` and `rhs` but never accesses the `op` field from `Binary { op, lhs, rhs }`. It constrains both sides to the same type and returns the LHS type — no operator-specific logic.

### Current behavior

```
x + "hello"  →  both sides unified, result is String (no error)
```

### Plan

1. **Extract the operator** from the resolved `Binary` expression. The function signature needs access to `op: hir::BinaryOp`.

2. **Match on operator kind** and generate constraints:

   | Operator | Operand constraint | Result type |
   |----------|--------------------|-------------|
   | `Add`, `Sub`, `Mul`, `Div` | Both operands unify with each other. Both must be numeric (`Int` or `Fraction`). | Same as operand type |
   | `Custom(path)` | Treat as a two-argument function call. Look up the function's type and unify `(lhs, rhs) → result`. | Return type of the function |
   | `Missing` | No constraints. Return a fresh type variable. | Fresh variable |

3. **Numeric constraint strategy** — The current constraint system uses type equations only (no trait constraints). Two approaches:

   **Option A — Fresh variable (current-compatible):**
   Constrain `lhs == rhs` via a shared fresh variable. The operands will unify with whatever concrete type they encounter. This catches `1 + "hi"` (Int vs String mismatch) but allows `x + y` where both are String (no "must be numeric" check).

   **Option B — Introduce a numeric constraint mechanism:**
   Add a concept of "type class constraints" to the equation solver. When arithmetic operators are used, add a `Numeric(var)` constraint. During solving, verify the variable resolves to `Int` or `Fraction`.

   **Recommendation:** Start with Option A for immediate improvement, then move to Option B when the trait/constraint system (from the generic type handling plan) is in place.

4. **Custom operator handling:**
   - Resolve `path` to find the operator function definition.
   - Get its type (should be a function type `a → b → c`).
   - Unify `lhs` with `a`, `rhs` with `b`, result with `c`.
   - Reuse the same logic as `infer_function_call`.

### Tests

- `1 + 2` — both Int, result Int.
- `1 + "hello"` — Int vs String mismatch error.
- `1.0 + 2.0` — both Fraction, result Fraction.
- `1 + 2.0` — Int vs Fraction mismatch error.
- Custom operator resolving to a function — should type-check like a function call.

---

## TODO 2: Unary operator constraints

**File:** `src/hir_ty/hm/constraint_gen/expr.rs:260`

### Problem

`infer_unary()` ignores the operator and returns the inner expression's type unchanged. Currently only `Neg` exists as a unary operator.

### Plan

1. **Extract the operator** from the resolved `Unary { op, expression }`.

2. **Match on operator kind:**

   | Operator | Operand constraint | Result type |
   |----------|--------------------|-------------|
   | `Neg` | Operand must be numeric (`Int` or `Fraction`) | Same as operand type |

3. **Same numeric constraint strategy as binary operators** — use Option A (unify with fresh variable, catch mismatches with non-numeric types) initially.

### Tests

- `-1` — Int, result Int.
- `-1.0` — Fraction, result Fraction.
- `-"hello"` — error, String is not numeric.
- `-x` where `x: Int` — should work.

---

## Implementation Steps

1. Modify `infer_binary` to accept/extract the `BinaryOp`.
2. Add match arms for `Add | Sub | Mul | Div` with `lhs == rhs` equation (Option A).
3. Add match arm for `Custom(path)` delegating to function call inference.
4. Add match arm for `Missing` returning a fresh variable.
5. Modify `infer_unary` to accept/extract the `UnaryOp`.
6. Add match arm for `Neg` constraining operand type.
7. Add test cases for each operator kind.
8. (Future) When trait constraints are available, upgrade to Option B with `Numeric` constraint.
