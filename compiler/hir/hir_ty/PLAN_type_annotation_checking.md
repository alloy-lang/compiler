# Plan: Type Annotation Constraint Checking

## Overview

One TODO for implementing trait constraint checking on built-in types, lambdas, and tuples.

---

## TODO: Implement constraint checking for built-in types, lambdas, etc.

**File:** `src/hir_ty/type_annotation_check.rs:167`

### Problem

`check_trait_constraints()` currently handles:
- **TypeDef** (user-defined types) — searches for behaviors that implement required traits.
- **Generic / ConstrainedGeneric** — accepts them (constraints checked at instantiation).
- **Everything else** — the wildcard `_ => Ok(())` accepts all types unconditionally.

This means built-in types (`Int`, `String`, `Bool`, etc.), lambda types, and tuple types are never checked against trait constraints. A `String` can pass a `Numeric` constraint without error.

### Plan

#### Step 1: Built-in type constraint checking

Define which traits each built-in type satisfies. Two approaches:

**Option A — Hardcoded registry:**

```rust
fn builtin_satisfies_trait(builtin: &BuiltInType, trait_fql: &Fql<hir::Trait>) -> bool {
    // Look up the trait name and check against known implementations
    match (builtin, trait_name) {
        (BuiltInType::Int, "Eq") => true,
        (BuiltInType::Int, "Ord") => true,
        (BuiltInType::Int, "Numeric") => true,
        (BuiltInType::String, "Eq") => true,
        (BuiltInType::String, "Ord") => true,
        (BuiltInType::Bool, "Eq") => true,
        _ => false,
    }
}
```

**Option B — Behavior-based (consistent with user-defined types):**

Allow behaviors to be defined for built-in types in the standard library:
```
behavior Int : Eq { ... }
behavior Int : Ord { ... }
```

Then reuse the existing `has_behavior_for_trait()` lookup for built-in types too.

**Recommendation:** Option B is more consistent and extensible. Built-in type behaviors should be defined in the stdlib. The type checker just needs to know how to find behaviors for built-in types, not just `TypeDef`s.

#### Step 2: Lambda type constraint checking

Lambda types (`a → b`) are function values. Determine which traits they can satisfy:

- Most traits (`Eq`, `Ord`, `Numeric`) — lambdas should **not** satisfy these. Return an error.
- A future `Callable` or `Fn` trait — lambdas would satisfy this.

For now, the implementation should reject lambdas for all trait constraints:

```rust
ResolvedType::Lambda { .. } => {
    Err(TypeError::TraitNotSatisfied { ty: found.clone(), trait_fql: constraint.clone() })
}
```

#### Step 3: Tuple type constraint checking

Tuple types `(a, b, c)` could satisfy a trait if all elements satisfy it. For example, `(Int, Int)` satisfies `Eq` if `Int` satisfies `Eq`.

```rust
ResolvedType::Tuple(elements) => {
    for element in elements {
        check_trait_constraints(db, module_id, element, constraints)?;
    }
    Ok(())
}
```

This is a recursive check — each element is checked against the same constraints.

#### Step 4: Unit type

`Unit` (the empty tuple / void type) trivially satisfies most constraints since there's only one value. Decide per-trait:

- `Eq` — yes (unit equals unit).
- `Ord` — yes (trivially ordered).
- `Numeric` — no.

Simplest: treat `Unit` like a built-in type with its own behavior set.

### Implementation steps

1. Extend `check_trait_constraints()` to dispatch on `BuiltIn`, `Lambda`, `Tuple`, and `Unit` instead of using the wildcard.
2. For `BuiltIn` — look up behaviors defined for that type (Option B) or use a registry (Option A).
3. For `Lambda` — reject all trait constraints for now.
4. For `Tuple` — recursively check each element.
5. For `Unit` — either use a registry or look up behaviors.
6. Add a proper `TypeError` variant for "type does not satisfy trait constraint".

### Tests

- `Int` satisfies `Eq` — should pass (once behavior is defined in stdlib).
- `String` satisfies `Numeric` — should fail.
- `(Int, Int)` satisfies `Eq` — should pass if `Int` satisfies `Eq`.
- `(Int, String)` satisfies `Numeric` — should fail (String doesn't satisfy Numeric).
- `fn(x) -> x` satisfies `Eq` — should fail.
- `Unit` satisfies `Eq` — should pass.

### Dependencies

- The trait/behavior system for built-in types may need stdlib changes.
- This plan interacts with the generic type handling plan — once constraints are tracked through inference, this checking becomes the enforcement point.
