# Plan: Pattern & Constructor Types

## Overview

Two TODOs related to type-checking patterns: looking up constructor type schemes for destructure patterns, and clarifying how pattern types interact with expression-level type annotations.

---

## TODO 1: Look up constructor type scheme from target and scope

**File:** `src/hir_ty/hm/constraint_gen/pattern.rs:75`

### Problem

`infer_destructure()` receives `target: Fql<hir::TypeDefinition>` and `args: &[Fql<hir::Pattern>]` but ignores the target entirely. It returns a fresh type variable with no constraints — the type of the pattern is completely unknown to the solver.

### Current behavior

```rust
fn infer_destructure(ctx, fql, _target, args) -> MonoType {
    let ty = ctx.fresh_type_var();  // No lookup, no constraints
    ctx.assign_type(fql, ty)
}
```

A pattern like `Option::Some(x)` gets a fresh variable instead of `Option[t]` with `x: t`.

### Existing infrastructure

The expression-side variant constructor logic in `constraint_gen/expr.rs` already does exactly what's needed:

- `build_constructor_type()` (expr.rs:299) — builds a curried function type for a constructor, mapping generic IDs to fresh type variables.
- Variant constructor inference (expr.rs:413) — looks up the type definition, finds the variant member, builds the constructor type, and handles polymorphism.

### Plan

1. **Get the variant name.** The resolved `Destructure` pattern contains `variant_name: hir::Name`. Pass this into `infer_destructure`.

2. **Look up the type definition** using `target`:
   ```rust
   let type_def = res::resolve_type_definition_by_id(db, target);
   ```

3. **Find the matching variant member** within the type definition by `variant_name`.

4. **Build the constructor type** using `build_constructor_type(ctx, &target, &member)` or equivalent logic:
   - Map generic parameters to fresh type variables.
   - Resolve each member field's type as a `MonoType`.
   - The constructor type is `field1 → field2 → ... → ResultType`.

5. **Decompose the constructor type** to get individual field types and the overall result type:
   - For a constructor `A(Int, String) → Option[Int]`:
     - Field types: `[Int, String]`
     - Result type: `Option[Int]`

6. **Constrain each argument pattern:**
   ```rust
   for (arg_pattern, field_type) in args.iter().zip(field_types) {
       let arg_ty = infer_pattern_hm(ctx, *arg_pattern);
       ctx.add_equation(arg_ty, field_type, fql.clone());
   }
   ```

7. **Return the result type** (the type being destructured, e.g., `Option[t]`):
   ```rust
   ctx.assign_type(fql, result_type)
   ```

8. **Handle arity mismatch** — if `args.len() != field_types.len()`, report an error.

### Tests

- `Some(x)` matching `Option[Int]` — `x` should infer as `Int`.
- `Pair(a, b)` matching `Pair[Int, String]` — `a: Int`, `b: String`.
- `None` (no args) matching `Option[t]` — result is `Option[t]`.
- Nested: `Some(Some(x))` — `x` should infer correctly through two layers.
- Wrong arity: `Some(a, b)` — should error.

---

## TODO 2: Pattern type annotations from expressions

**File:** `src/hir_ty/hm/inference.rs:210`

### Problem

The comment notes that patterns cannot have type annotations directly in the syntax, but their types can be specified by annotations on the enclosing expression. The call to `check_type_annotation()` on patterns may be redundant or incorrectly applied.

### Context

When a user writes:
```
typeof x : Int
let x = 1
```

The annotation is on the expression (`let x = ...`), not on the pattern (`x`). The pattern `x` gets its type from the expression's annotation.

### Plan

1. **Audit the call to `check_type_annotation()` for patterns** — determine if it's ever triggered and whether it produces correct results.
2. **Clarify the flow:** Expression annotations should propagate to patterns through unification (the pattern variable and the expression share a type variable). If this already works through the constraint system, the explicit check may be unnecessary.
3. **If redundant:** Remove the call for patterns and add a comment explaining that pattern types are constrained through expression annotations via unification.
4. **If needed:** Keep the call but ensure it handles the indirection correctly — the annotation comes from the expression, not the pattern itself.

### Tests

- `typeof x : Int; let x = 1` — `x` pattern should have type `Int`.
- `typeof (a, b) : (Int, String); let (a, b) = (1, "hi")` — tuple pattern elements should get correct types.
- Conflicting annotation: `typeof x : String; let x = 1` — should produce one clear error, not duplicates.

---

## Implementation Order

1. **TODO 1** — Implement constructor type lookup in `infer_destructure`. This is the higher-priority item as it blocks correct type inference for all pattern matching on algebraic data types.
2. **TODO 2** — Audit and clarify the pattern annotation check. This is a correctness/cleanup task that can be addressed after constructor patterns work.
