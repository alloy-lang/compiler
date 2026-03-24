# Plan: Generic Type Handling

## Overview

Four TODOs related to tracking generic type variables and their constraints through inference, annotation checking, and output.

The codebase has `ConstrainedGeneric` variants in both `InferredType` (hir_infer) and `ResolvedType` (hir_ty), and a `TypeConstraint` struct in the HM inference context — but the constraint tracking infrastructure is not yet functional.

---

## TODO 1: Track generic type variable assignments for consistency

**File:** `compiler/hir/hir_ty/src/validation/type_annotation.rs` — `check_type_compatibility()`

### Problem

`check_type_compatibility()` currently allows any two generic type variables to match, regardless of ID. For an annotation like `forall a. (a, a)`, the inferred type `(Generic(0), Generic(1))` would incorrectly pass — both tuple elements must be the same generic.

### Plan

1. Add a `generic_mapping: &mut FxHashMap<usize, usize>` parameter to `check_type_compatibility()`, mapping annotation generic IDs to inferred generic IDs.
2. In the `(TypeVar/Generic, Generic)` arms: if the annotation ID is already mapped, verify it maps to the same inferred ID.
3. Thread this map through all recursive calls.

### Tests

- Annotation `(a, a)` with inferred `(Generic(0), Generic(1))` — should fail.
- Annotation `(a, b)` with inferred `(Generic(0), Generic(1))` — should pass.

---

## TODO 2: Track that generics have constraints

**File:** `compiler/hir/hir_ty/src/validation/type_annotation.rs`

### Problem

When matching `(ConstrainedTypeVar, Generic)`, constraints are not recorded. The unconstrained generic should inherit the annotation's constraints for downstream consumers.

### Plan

1. Extend the mapping from TODO 1 to record constraints alongside variable mappings.
2. Use the constraint info when converting results — upgrade `Generic(id)` to `ConstrainedGeneric { id, constraints }` where applicable.

---

## TODO 3: Track constraints and enforce them during solving

**Files:**
- `compiler/hir/hir_infer/src/hir_ty/hm/mod.rs` — `HMInferenceContext`
- `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs` — `annotated_to_mono()`
- `compiler/hir/hir_infer/src/hir_ty/hm/constraint_gen/expr/mod.rs` — `inferred_to_mono()`

### Problem

`annotated_to_mono()` and `inferred_to_mono()` convert `ConstrainedTypeVar`/`ConstrainedGeneric` into bare `MonoType::Var(fresh_id)`, discarding the constraints entirely. The `TypeConstraint` struct and `constraints` field on `PolyType` exist but are never populated.

### Plan

1. Add a constraint store to `HMInferenceContext`:
   ```rust
   constraint_store: FxHashMap<TypeVarId, Vec<TypeConstraint>>
   ```
2. In `annotated_to_mono()`, when handling `ConstrainedTypeVar`, record constraints in the store.
3. During unification, when a constrained variable is unified with a concrete type, verify the type satisfies each constraint.
4. When two constrained variables unify, merge their constraint sets.

### Tests

- Constrained generic unified with a type that implements the trait — should pass.
- Constrained generic unified with a type missing the trait — should error.

---

## TODO 4: Create ConstrainedGeneric on output

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/inference.rs` — `mono_to_resolved_with_map()`

### Problem

`mono_to_resolved_with_map()` always converts `MonoType::Var(id)` to `InferredType::Generic(n)`, even when the variable has constraints in the store. There is a TODO comment at line ~289 noting this.

### Plan

1. Pass `&constraint_store` into `mono_to_resolved_with_map()`.
2. When converting `MonoType::Var(id)`, check for constraints:
   ```rust
   if let Some(constraints) = constraint_store.get(&id) {
       InferredType::ConstrainedGeneric { id: generic_id, constraints: constraints.clone() }
   } else {
       InferredType::Generic(generic_id)
   }
   ```

### Tests

- A function with constrained annotation should produce `ConstrainedGeneric` in its inferred type.
- A function with unconstrained annotation should produce `Generic`.

---

## Implementation Order

1. **TODO 3** — Add constraint store and preserve constraints during mono conversion.
2. **TODO 4** — Output `ConstrainedGeneric` in `mono_to_resolved_with_map()`.
3. **TODO 3 (part 2)** — Enforce constraints during unification.
4. **TODO 1** — Add generic variable assignment tracking in annotation checking.
5. **TODO 2** — Propagate constraints from annotation checking back to inferred types.
