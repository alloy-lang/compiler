# Plan: Infrastructure & Miscellaneous

## Overview

Four remaining TODOs covering error deduplication, test infrastructure decisions, compiler pipeline integration, and interner bootstrapping.

Stdlib tests have been enabled and are passing in both `alloy_hir_infer` and `alloy_hir_typed`.

---

## TODO 1: Add error deduplication if needed

**File:** `compiler/hir/hir_ty/src/lib.rs` — `type_check_module()`

### Problem

`type_check_module()` collects inference errors from `alloy_hir_infer` and validation errors from the validation layer. No deduplication is performed, leading to duplicate errors for the same issue (e.g., both a `UnificationError` and `ConflictingTypeAnnotation` for the same range).

### Plan

See `PLAN_error_reporting.md` TODO 1 for the deduplication strategy. The deduplication should happen in `type_check_module()` as the final step before returning.

---

## TODO 2: Decide if tests should fail on warnings

**Files:** `compiler/hir/hir_infer/src/tests.rs` and `compiler/hir/hir_ty/src/tests.rs`

### Problem

No warnings are currently emitted (see `PLAN_error_reporting.md` TODO 2), but once they are, the test infrastructure needs a policy on whether warnings fail tests.

### Plan

1. Uncomment/add warning assertion blocks once the warning system is functional.
2. Default: `fail_on_warnings = true` for new tests.
3. Existing tests that legitimately produce warnings can opt out via naming convention or parameter.

### Dependencies

- Requires at least one warning variant to be defined.

---

## TODO 3: Phase 3 & 4 compiler integration

**File:** `compiler/src/lib.rs` — `compile()`

### Problem

The `compile()` function only runs Phases 1-2 (parsing and HIR lowering). Type checking (Phase 3) and IR generation (Phase 4) are not integrated, as noted by TODO comments at lines 69-70.

### Plan

#### Phase 3 integration

1. After all modules are lowered, run type checking on each:
   ```rust
   for module_id in &module_ids {
       let typed_module = alloy_hir_ty::type_check_module(db, *module_id);
       // Collect type errors
   }
   ```
2. Salsa handles cross-module dependency resolution automatically.
3. If any errors are present, report them and stop compilation.

#### Phase 4 integration (future)

1. Reachability analysis from entrypoints.
2. IR generation for reachable definitions.
3. Backend code generation (not yet designed).

### Implementation steps (Phase 3 only)

1. Add `alloy_hir_ty` as a dependency of the `compiler` crate.
2. Ensure `CompilerDatabase` implements `HirTyDatabase`.
3. Call `type_check_module()` for each module after lowering.
4. Collect and report type errors alongside parse/lowering errors.

### Dependencies

- Type checker should be reasonably stable before integration.

---

## TODO 4: Add builtin types to the interner

**File:** `compiler/interner/src/lib.rs`

### Problem

The interner's `Default` implementation has a TODO to pre-intern builtin type names. Currently only a commented-out example exists.

### Plan

1. Pre-intern all builtin type names:
   ```rust
   impl Default for Interner {
       fn default() -> Self {
           let mut interner = Self(lasso::Rodeo::default());
           interner.intern("Int");
           interner.intern("Fraction");
           interner.intern("String");
           interner.intern("Char");
           interner.intern("Bool");
           interner
       }
   }
   ```
2. Optionally expose well-known keys as constants for fast lookup.

### Dependencies

None — standalone change.

---

## Implementation Order

1. **TODO 4** — Pre-intern builtin types. Trivial, no dependencies.
2. **TODO 1** — Error deduplication. Implement after the error reporting plan.
3. **TODO 2** — Warning failure policy. After warnings exist.
4. **TODO 3** — Pipeline integration. After type checker is stable.
