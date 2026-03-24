# Plan: Type Enum Consolidation

## Overview

The type inference and checking pipeline uses 5 type enum representations with 6+ conversions between them. Two of these are redundant and a large body of code is a dead fork. This plan consolidates where possible while preserving the separations that are architecturally motivated.

---

## Current State: 5 Type Enums

### 1. `AnnotatedType` (hir_resolved)

**File:** `compiler/hir/hir_resolved/src/annotated_type.rs`

The resolved form of user-written type annotations. Uses Fql-based identity for type variables (each type variable points back to its declaration site).

**Unique variants:** `TypeVar { fql, name }`, `ConstrainedTypeVar { fql, name, constraints }`, `SelfType { trait_fql, ... }`

**Role:** Input to inference. Produced by `resolve_annotated_type()` (Salsa tracked).

### 2. `MonoType` (hir_infer) -- active

**File:** `compiler/hir/hir_infer/src/hir_ty/hm/mod.rs`

Internal HM representation using ephemeral `TypeVarId`s. Created fresh for each inference run. Includes `Var(TypeVarId)`, `App { constructor, args }`, `TypeDef { type_args: Vec<TypeVarId> }`.

**Role:** Internal to the HM algorithm. Lives and dies within a single `infer_body_type` or `infer_types_hm` call.

### 3. `MonoType` (hir_ty) -- DEAD FORK

**File:** `compiler/hir/hir_ty/src/hir_ty/hm/mod.rs`

A forked copy of hir_infer's MonoType. Structurally identical but lives in a separate crate with its own `HMInferenceContext` that has extra fields (`expr_to_group`, `current_group`, `env_type_vars`, `instantiations`).

**Role:** None. `type_check_module` calls `alloy_hir_infer::infer_types_module`, not the hir_ty HM pipeline.

### 4. `InferredType` (hir_infer)

**File:** `compiler/hir/hir_infer/src/hir_ty/mod.rs`

The public output of hir_infer. Uses sequential `Generic(usize)` IDs (not TypeVarIds). Variants: `Unconstrained`, `Missing`, `Unit`, `TypeDef`, `BuiltIn`, `Lambda`, `Tuple`, `Bounded`, `Generic`, `ConstrainedGeneric`.

**Role:** Output of inference, input to type checking/validation.

### 5. `ResolvedType` (hir_ty) -- REDUNDANT

**File:** `compiler/hir/hir_ty/src/hir_ty/mod.rs`

Nearly identical to `InferredType`. Has all the same variants plus `UnknownReference(Fql<TypeReference>)`. Connected by a mechanical `From<&InferredType>` impl that deep-clones every variant.

**Role:** Output of type checking. Used by validation and downstream consumers.

---

## Conversions

```
AnnotatedType ──annotated_to_mono──→ MonoType (hir_infer)
AnnotatedType ──annotated_to_mono──→ MonoType (hir_ty, dead fork)
AnnotatedType ──annotated_to_inferred──→ InferredType

InferredType ──inferred_to_mono──→ MonoType (importing cross-module signatures)
MonoType ──mono_to_resolved_with_map──→ InferredType (hir_infer, after solving)
MonoType ──mono_to_resolved_with_map──→ ResolvedType (hir_ty, dead fork)

InferredType ──From impl──→ ResolvedType (1:1 mechanical deep clone)
```

---

## Changes

### Change 1: Delete the hir_ty HM fork

**Delete entire directory:** `compiler/hir/hir_ty/src/hir_ty/hm/`

This removes:
- Forked `MonoType`, `PolyType`, `TypeVarId`, `TypeVarGenerator`, `TypeConstraint`, `TypeEquation`
- Forked `HMInferenceContext` (with extra fields not in hir_infer)
- Forked `unification.rs` (Substitution, unify_types, solve_equations)
- Forked `constraint_gen/` (expr.rs, pattern.rs, mod.rs)
- Forked `inference.rs` (infer_types_hm, annotated_to_mono, mono_to_resolved_with_map)

**Update:** `compiler/hir/hir_ty/src/hir_ty/mod.rs` -- remove `mod hm` and the `infer_types` function that called `hm::infer_types_hm`. This function is already unused by `type_check_module`.

**Verification:** `type_check_module` in `lib.rs` already calls `alloy_hir_infer::infer_types_module` directly. The forked HM pipeline is not on any active code path.

### Change 2: Merge `InferredType` and `ResolvedType`

**Option A -- hir_ty reuses InferredType directly:**

1. Add `UnknownReference` variant to `InferredType` in hir_infer
2. Change `HirTypedModule` to store `InferredType` instead of `ResolvedType`
3. Change `validate_type_annotations` and `validate_behaviors` to work with `InferredType`
4. Delete `ResolvedType` enum and its `From` impl
5. Re-export `InferredType` from hir_ty (or just have consumers use hir_infer directly)

**Option B -- move the shared type to a new location:**

1. Create a shared `Type` enum (or keep the name `InferredType`) in hir_infer
2. Add the `UnknownReference` variant
3. Both hir_infer and hir_ty use the same type
4. Delete `ResolvedType`

**Recommendation:** Option A is simpler. `UnknownReference` is only used by validation in hir_ty, and adding it to `InferredType` is a one-variant addition. The name `InferredType` works fine for both crates.

**Files to update:**
- `compiler/hir/hir_infer/src/hir_ty/mod.rs` -- add `UnknownReference` variant
- `compiler/hir/hir_ty/src/hir_ty/mod.rs` -- delete `ResolvedType`, re-export `InferredType`
- `compiler/hir/hir_ty/src/lib.rs` -- change `HirTypedModule` to use `InferredType`
- `compiler/hir/hir_ty/src/validation/type_annotation.rs` -- s/ResolvedType/InferredType/
- `compiler/hir/hir_ty/src/validation/behavior.rs` -- s/ResolvedType/InferredType/
- `compiler/hir/hir_ty/src/diagnostics.rs` -- s/ResolvedType/InferredType/
- `compiler/hir/hir_ty/src/tests.rs` -- s/ResolvedType/InferredType/
- Test files in `compiler/hir/hir_ty/src/tests/` -- update type references in snapshots

### No change: types that stay separate

**`AnnotatedType`** stays in hir_resolved. It carries source-level Fql identity for type variables that InferredType doesn't need. It's the bridge between name resolution and inference.

**`MonoType` / `PolyType`** stay in hir_infer as crate-private types. They use ephemeral `TypeVarId`s that are meaningless outside a single inference run. Exposing them would leak implementation details and prevent the per-definition Salsa refactor (see `PLAN_per_definition_inference.md`).

### No change: boundary conversions that remain

These conversions cross representation boundaries and can't be eliminated without merging the internal and external representations (which would be worse):

- `annotated_to_mono` -- enters the HM world (Fql identity -> TypeVarId)
- `mono_to_resolved_with_map` -- exits the HM world (TypeVarId -> Generic(usize))
- `inferred_to_mono` -- re-enters the HM world (importing cross-module signatures)
- `annotated_to_inferred` -- direct annotation-to-output (skips HM for annotated signatures)

---

## Implementation Order

1. **Delete the hir_ty HM fork** -- pure deletion, no behavioral change, no test updates needed
2. **Merge InferredType and ResolvedType** -- mechanical rename + add one variant

---

## Verification

```bash
# After Change 1 (delete fork):
cargo test --color=always --no-fail-fast --lib -p alloy_hir_typed -- --no-capture

# After Change 2 (merge types):
cargo test --color=always --no-fail-fast --workspace -- --no-capture
```
