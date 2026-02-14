# Plan: Bounded Types & Traits

## Overview

Two TODOs in the `hir_resolved` layer: validating type arguments on bounded type references, and supporting function calls to abstract trait members.

---

## TODO 1: Bounded type reference should check args

**File:** `../hir_resolved/src/type_definition.rs:86`

### Problem

When resolving a bounded type reference like `List[Int, String]`, only the base type (`List`) is resolved. The type arguments (`Int, String`) are completely ignored — no validation that:
- The base type accepts generic parameters at all.
- The number of arguments matches the number of parameters.
- Each argument is itself a valid type.

### Current behavior

```rust
hir::TypeReference::Bounded { base, args: _ } => {
    resolve_type_definition_by_ref_id(db, module_id, *base)
}
```

### Plan

1. **Resolve the base type** as before to get the `TypeDefinition`.

2. **Get the expected parameter count** from the type definition. Type definitions with generic parameters should expose how many they expect.

3. **Check arity:**
   ```rust
   let expected_count = type_def.generic_param_count();
   let actual_count = args.len();
   if expected_count != actual_count {
       // Report arity mismatch error
   }
   ```

4. **Validate each argument** by resolving it as a type reference:
   ```rust
   for arg in args {
       resolve_type_by_ref_id(db, module_id, *arg)?;
   }
   ```

5. **Decide the return type.** Currently the function returns `Option<Fql<hir::TypeDefinition>>` — it only identifies the base type. To also return resolved arguments, either:
   - Change the return type to include resolved args.
   - Keep this function as-is (just validation) and let the type checker handle the full bounded type in Phase 3.

   **Recommendation:** Keep the function signature unchanged. This function's job is to find the `TypeDefinition` for name resolution. Argument validation can be a separate step, or deferred to Phase 3 where full type information is available.

6. **Add error reporting** for arity mismatches and invalid arguments. Use the existing error infrastructure in the resolution layer.

### Tests

- `Option[Int]` — valid, Option has 1 parameter.
- `Option[Int, String]` — arity error, too many arguments.
- `Option` (no args when expected) — may be valid (uninstantiated generic) or error depending on context.
- `Int[String]` — error, Int is a built-in with no parameters.
- Nested: `Option[List[Int]]` — should validate recursively.

---

## TODO 2: Function calls to abstract trait members

**File:** `../hir_resolved/src/expr.rs:195`

### Problem

When resolving a function call target (e.g., `foo(1, 2)`), the resolver tries several strategies:
1. Look up as an expression variable.
2. Look up as a pattern variable.
3. Look up as a type constructor.

It does **not** try looking up as an abstract trait member. The code for this is commented out:

```rust
// TODO: function calls to abstract trait members
// if let Some(expr) = resolve_abstract_trait_member_by_path(db, module_id, target) {
//     return Err(Ok(expr));
// }
```

### Context

Abstract trait members are type signatures without implementations:
```
trait Showable {
    show : Self -> String    // abstract — no body
    display : Self -> String // concrete — has body
}
```

When code inside a trait or behavior references `show(x)`, it should resolve to the abstract member. The implementation is dispatched at the call site based on the concrete type.

### Plan

1. **Uncomment the resolution code** and adapt it to the current function signature.

2. **Return an `AbstractTraitMemberRef` expression:**
   ```rust
   Expression::AbstractTraitMemberRef {
       trait_fql: Fql<hir::Trait>,
       member_name: hir::Name,
       type_annotation: Fql<hir::TypeAnnotation>,
   }
   ```

3. **Handle in constraint generation** (`hir_ty/src/hir_ty/hm/constraint_gen/expr.rs`):
   - When encountering an `AbstractTraitMemberRef` in a function call:
     - Look up the member's type annotation to get the function type.
     - Replace `Self` in the type with a fresh type variable (or the enclosing type for behaviors).
     - Generate constraints as if it were a regular function call.

4. **Determine call context:**
   - Inside a **behavior implementation**: `Self` is the concrete type being implemented. The call dispatches to the behavior's implementation.
   - Inside a **trait definition** (default method): `Self` is constrained by the trait. The call is abstract and dispatched at instantiation.
   - Outside trait/behavior: This should be an error — abstract members can't be called without a concrete type.

5. **Error cases:**
   - Calling an abstract member outside a trait/behavior scope — error.
   - Calling with wrong argument types — normal type mismatch error.

### Tests

- Inside a behavior: `show(self)` should resolve and type-check against the member's annotation.
- Inside a trait default method: `show(self)` should resolve with `Self` as a constrained generic.
- Outside trait/behavior: `show(x)` should produce an error.
- Type mismatch: calling abstract member with wrong argument type — should error.

---

## Implementation Order

1. **TODO 1** — Bounded type argument validation. This is a simpler, more isolated change.
2. **TODO 2** — Abstract trait member calls. This requires changes in both `hir_resolved` and `hir_ty` and is a more involved feature.
