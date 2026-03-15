# Plan: Generic Type Handling

## Overview

Four TODOs related to tracking generic type variables and their constraints through inference, annotation checking, and output.

---

## TODO 1: Track generic type variable assignments for consistency

**File:** `src/hir_ty/type_annotation_check.rs:72`

### Problem

`check_type_compatibility()` currently allows any two generic type variables to match, regardless of ID. For an annotation like `forall a. (a, a)`, the inferred type `(Generic(0), Generic(1))` would incorrectly pass — both tuple elements must be the same generic.

### Plan

1. Add a `generic_mapping: &mut FxHashMap<usize, usize>` parameter to `check_type_compatibility()`, mapping expected generic IDs to found generic IDs.
2. In the `(Generic(expected_id), Generic(found_id))` arm:
   - If `expected_id` is already in the map, verify it maps to `found_id`. Return a `TypeError` if not.
   - Otherwise, insert `expected_id → found_id`.
3. Thread this map through all recursive calls (Lambda arg/return, Tuple elements, Bounded args).
4. Initialize the map as empty at each top-level call site.

### Tests

- Annotation `(a, a)` with inferred `(Int, Int)` — should pass.
- Annotation `(a, a)` with inferred `(Int, String)` — should fail.
- Annotation `(a, b)` with inferred `(Int, String)` — should pass.
- Nested: `(a, (a, b))` with `(Int, (Int, String))` — should pass.
- Nested: `(a, (a, b))` with `(Int, (String, String))` — should fail.

---

## TODO 2: Track that generics have constraints

**File:** `src/hir_ty/type_annotation_check.rs:83`

### Problem

When matching `(Generic(_), ConstrainedGeneric { .. })`, the code returns `Ok(())` without recording that the unconstrained generic should inherit the annotation's constraints. Downstream consumers never learn that the generic must satisfy certain traits.

### Plan

1. Extend the mapping from TODO 1 to also record constraints: `FxHashMap<usize, (usize, Vec<Fql<hir::Trait>>)>`.
2. In the `(Generic(id), ConstrainedGeneric { id: cid, constraints })` arm:
   - Store the constraint set alongside the variable mapping.
3. Return this constraint mapping from `check_type_compatibility()` so the caller can propagate constraints into the `HirTypedModule` output or annotate the inferred type.
4. Use the constraint info when converting back to `ResolvedType` — upgrade `Generic(id)` to `ConstrainedGeneric { id, constraints }` where applicable.

### Tests

- Annotation `a : Eq` matched with inferred `Generic(0)` — constraint should propagate.
- Multiple constraints `a : Eq + Ord` — both should propagate.

---

## TODO 3: Track constraints and enforce them during solving

**File:** `src/hir_ty/hm/inference.rs:283`

### Problem

`resolved_to_mono()` converts `ConstrainedGeneric { id, constraints }` into a bare `MonoType::Var(fresh_id)`, discarding the constraints entirely. The solver never enforces them.

### Plan

1. Add a constraint store to `HMInferenceContext`:
   ```rust
   constraint_store: FxHashMap<TypeVarId, Vec<TypeConstraint>>
   ```
2. In `resolved_to_mono()`, when handling `ConstrainedGeneric`:
   ```rust
   let var = ctx.fresh_type_var();
   ctx.constraint_store.insert(var_id, constraints.to_vec());
   Some(var)
   ```
3. During unification (`solve_equations`), when a constrained variable is unified with a concrete type:
   - Look up the variable's constraints in the store.
   - Verify the concrete type satisfies each constraint (reuse `check_trait_constraints()` from `type_annotation_check.rs`).
   - If not satisfied, emit a `MissingTraitImplementation` error.
4. When two constrained variables unify, merge their constraint sets.

### Tests

- Constrained generic unified with a type that implements the trait — should pass.
- Constrained generic unified with a type missing the trait — should error.
- Two constrained generics unified — constraints should merge.

---

## TODO 4: Create ConstrainedGeneric on output

**File:** `src/hir_ty/hm/inference.rs:317`

### Problem

`mono_to_resolved_with_map()` always converts `MonoType::Var(id)` to `ResolvedType::Generic(n)`, even when the variable has constraints in the store.

### Plan

1. Pass `&constraint_store` into `mono_to_resolved_with_map()`.
2. When converting `MonoType::Var(id)`:
   ```rust
   if let Some(constraints) = constraint_store.get(&id) {
       ResolvedType::ConstrainedGeneric { id: generic_id, constraints: constraints.clone() }
   } else {
       ResolvedType::Generic(generic_id)
   }
   ```
3. Ensure that substitution propagates constraints: when `Var(a)` is substituted for `Var(b)`, merge constraint sets.

### Tests

- A function with constrained annotation should produce `ConstrainedGeneric` in its resolved type.
- A function with unconstrained annotation should produce `Generic`.

---

## Implementation Order

1. **TODO 3** — Add constraint store to context and preserve constraints in `resolved_to_mono()`.
2. **TODO 4** — Output `ConstrainedGeneric` in `mono_to_resolved_with_map()`.
3. **TODO 3 (part 2)** — Enforce constraints during unification.
4. **TODO 1** — Add generic variable assignment tracking in annotation checking.
5. **TODO 2** — Propagate constraints from annotation checking back to inferred types.

This order builds the infrastructure first (store, output) before layering enforcement and annotation-driven propagation on top.
