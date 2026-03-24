# Plan: Pattern & Constructor Types

## Overview

Two TODOs related to type-checking patterns: looking up constructor type schemes for destructure patterns, and clarifying how pattern types interact with expression-level type annotations.

---

## TODO 1: Look up constructor type scheme from target and scope

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/constraint_gen/pattern.rs` — `infer_destructure()`

### Problem

`infer_destructure()` receives `target: Fql<hir::TypeDefinition>` and `args: &[Fql<hir::Pattern>]` but ignores the target entirely. It returns a fresh type variable with no constraints — the type of the pattern is completely unknown to the solver.

### Current behavior

```rust
fn infer_destructure(ctx, fql, _target, args) -> MonoType {
    let _field_types = args.iter()
        .map(|field_id| infer_pattern_hm(ctx, field_id.clone()))
        .collect::<Vec<_>>();
    // TODO: look up the constructor's type scheme from target and scope
    let ty = ctx.fresh_type_var();
    ctx.assign_type(fql, ty)
}
```

A pattern like `Option::Some(x)` gets a fresh variable instead of `Option[t]` with `x: t`.

### Existing infrastructure

The expression-side variant constructor logic already does what's needed:
- `infer_data_constructor()` and `infer_variant_constructor()` in `constraint_gen/expr/type_def.rs` build constructor types with proper generic mappings.

### Plan

1. **Look up the type definition** using `target`:
   ```rust
   let type_def = res::resolve_type_definition_by_id(db, target);
   ```

2. **Build the constructor type** using logic similar to expression-side constructors:
   - Map generic parameters to fresh type variables.
   - Resolve each member field's type as a `MonoType`.

3. **Decompose the constructor type** to get individual field types and the overall result type.

4. **Constrain each argument pattern:**
   ```rust
   for (arg_pattern, field_type) in args.iter().zip(field_types) {
       let arg_ty = infer_pattern_hm(ctx, *arg_pattern);
       ctx.add_equation(arg_ty, field_type, fql.clone());
   }
   ```

5. **Return the result type** (e.g., `Option[t]`).

6. **Handle arity mismatch** — if `args.len() != field_types.len()`, report an error.

### Tests

- `Some(x)` matching `Option[Int]` — `x` should infer as `Int`.
- `Pair(a, b)` matching `Pair[Int, String]` — `a: Int`, `b: String`.
- Nested: `Some(Some(x))` — `x` should infer correctly through two layers.
- Wrong arity: `Some(a, b)` — should error.

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
