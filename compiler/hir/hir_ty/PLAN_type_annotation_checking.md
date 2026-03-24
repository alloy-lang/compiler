# Plan: Type Annotation Constraint Checking

## Overview

One TODO for implementing trait constraint checking on built-in types, lambdas, and tuples.

---

## TODO: Implement constraint checking for built-in types, lambdas, etc.

**File:** `compiler/hir/hir_ty/src/validation/type_annotation.rs` — `check_trait_constraints()`

### Problem

`check_trait_constraints()` currently handles:
- **TypeDef** (user-defined types) — searches for behaviors that implement required traits.
- **Generic / ConstrainedGeneric** — accepts them (constraints checked at instantiation).
- **Everything else** — the wildcard `_ => Ok(())` accepts all types unconditionally.

This means built-in types (`Int`, `String`, `Bool`, etc.), lambda types, and tuple types are never checked against trait constraints.

### Plan

#### Step 1: Built-in type constraint checking

**Option A — Hardcoded registry:**
```rust
fn builtin_satisfies_trait(builtin: &BuiltInType, trait_name: &str) -> bool {
    match (builtin, trait_name) {
        (BuiltInType::Int, "Eq" | "Ord" | "Numeric") => true,
        (BuiltInType::String, "Eq" | "Ord") => true,
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
Then reuse the existing `has_behavior_for_trait()` lookup.

**Recommendation:** Option B is more consistent and extensible.

#### Step 2: Lambda type constraint checking

Lambda types should reject all trait constraints for now:
```rust
ResolvedType::Lambda { .. } => {
    Err(ConflictingTypeAnnotationReason::TraitNotSatisfied { .. })
}
```

#### Step 3: Tuple type constraint checking

Recursively check each element against the same constraints:
```rust
ResolvedType::Tuple(elements) => {
    for element in elements {
        check_trait_constraints(db, element, constraints)?;
    }
    Ok(())
}
```

#### Step 4: Unit type

Treat `Unit` like a built-in type with its own behavior set.

### Implementation steps

1. Extend `check_trait_constraints()` to dispatch on `BuiltIn`, `Lambda`, `Tuple`, and `Unit`.
2. Add a proper error variant for "type does not satisfy trait constraint".
3. Add test cases for each type kind.

### Dependencies

- The trait/behavior system for built-in types may need stdlib changes.
- Interacts with `PLAN_generic_type_handling.md` — once constraints are tracked through inference, this checking becomes the enforcement point.
