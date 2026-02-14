# Plan: Infrastructure & Miscellaneous

## Overview

Five TODOs covering error deduplication in the module, test infrastructure decisions, stdlib test fixes, compiler pipeline integration, and interner bootstrapping.

---

## TODO 1: Add error deduplication if needed

**File:** `src/lib.rs:76`

### Problem

`HirTypedModule` collects errors via `push_error()` and `error()` but performs no deduplication. Duplicate errors may reach consumers (LSP, CLI).

### Plan

This is closely related to the error reporting plan (see `PLAN_error_reporting.md`). The deduplication should happen here as the final step before errors are exposed:

1. Add a `deduplicate_errors()` method on `HirTypedModule`.
2. Call it at the end of type checking, before returning the module.
3. Deduplication strategy: remove errors with identical `(range, error_code)` pairs, keeping the more specific variant (e.g., prefer `ConflictingTypeAnnotation` over `UnificationError`).

### Dependencies

Implement after the Phase 1 deduplication from `PLAN_error_reporting.md`.

---

## TODO 2: Decide if tests should fail on warnings

**File:** `src/tests.rs:157,176`

### Problem

Two commented-out assertion blocks would fail tests when warnings are present — one for lowering warnings, one for type checking warnings. Currently neither is active.

### Plan

1. **Uncomment both assertion blocks** once the warning system is functional (see `PLAN_error_reporting.md` TODO 2 for warning kinds).

2. **Add a parameter** to the test runner for flexibility:
   ```rust
   pub fn run_hir_ty_test(
       path: &Path,
       input: &str,
       expect_parse_errors: bool,
       expect_lowering_errors: bool,
       expect_type_checking_errors: bool,
       fail_on_warnings: bool,  // new
   ) -> String
   ```

3. **Default behavior:** `fail_on_warnings = true` for new tests. Existing tests that legitimately produce warnings can opt out.

4. **Alternative:** Use a test-level attribute or naming convention (e.g., files in a `warnings_ok/` directory don't fail on warnings).

### Dependencies

- Requires at least one warning variant to be defined.
- Should be done after stdlib test fixes (TODO 3) to avoid immediate breakage.

---

## TODO 3: Continue fixing lowering errors in stdlib

**File:** `src/tests.rs:195`

### Problem

The `test_std_lib()` test is entirely commented out because stdlib files have lowering errors. This blocks:
- Validating type checking against real-world code.
- Enabling the warning failure policy.
- Confidence in compiler correctness.

### Plan

1. **Uncomment the test** and run it to get the full list of failing stdlib files.

2. **Triage the errors:**
   - Lowering errors (Phase 2) — fix in the HIR lowering pass or in the stdlib source.
   - Type checking errors (Phase 3) — may indicate missing features rather than bugs.

3. **Fix incrementally:**
   - Start with the simplest stdlib files.
   - Fix one file at a time, running the test after each fix.
   - Track progress with a checklist of stdlib files.

4. **If some files need features not yet implemented** (e.g., trait constraints, bounded type args), mark them as `#[ignore]` with a comment referencing the blocking TODO, rather than commenting out the entire test.

### Implementation steps

1. Uncomment `test_std_lib()`.
2. Run `cargo test test_std_lib` and capture output.
3. Categorize failures.
4. Fix or `#[ignore]` each failing file.
5. Keep the test permanently enabled.

---

## TODO 4: Phase 3 & 4 compiler integration

**File:** `../../compiler/src/lib.rs:69-70`

### Problem

The `compile()` function only runs Phases 1-2 (parsing and lowering). Type checking (Phase 3) and IR generation (Phase 4) are not integrated.

### Plan

#### Phase 3 integration

1. After all modules are lowered, run type checking on each:
   ```rust
   let mut typed_modules = Vec::new();
   for module_id in &module_ids {
       let typed_module = alloy_hir_ty::type_check_module(db, *module_id);
       typed_modules.push(typed_module);
   }
   ```

2. Collect type errors alongside parse/lowering errors.

3. If any errors are present, report them and stop compilation (don't proceed to Phase 4).

4. Consider module ordering — if type checking depends on other modules' types (cross-module references), modules may need to be checked in dependency order or use a two-pass approach.

#### Phase 4 integration (future)

1. **Reachability analysis:**
   - Identify entrypoint(s) based on `CompilationTarget`.
   - For binaries: the `main` function in the entrypoint module.
   - For libraries: all public exports.
   - Walk the call graph from entrypoints to find reachable definitions.

2. **IR generation:**
   - Only generate IR for reachable definitions.
   - This requires an IR representation (not yet designed).
   - Depends on Phase 3 being complete and stable.

3. **Backend:**
   - Code generation from IR to target (LLVM, WASM, bytecode, etc.).
   - Not yet designed — out of scope for current TODOs.

### Implementation steps (Phase 3 only)

1. Add `alloy_hir_ty` as a dependency of the `compiler` crate.
2. Call `type_check_module()` for each module after lowering.
3. Collect and report type errors.
4. Gate further compilation on type error count.

### Dependencies

- Type checker should be reasonably stable before integration.
- Cross-module type checking may need additional work.

---

## TODO 5: Add builtin types to the interner

**File:** `../../compiler/interner/src/lib.rs:6`

### Problem

The interner's `Default` implementation doesn't pre-intern builtin type names. This means:
- Builtin type names get different keys across compilation runs.
- No guaranteed constant-time lookup for common names.
- Can't use well-known keys for builtin types.

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

2. Optionally expose well-known keys as constants:
   ```rust
   impl Interner {
       pub fn int_key(&self) -> Key { /* the key for "Int" */ }
       pub fn string_key(&self) -> Key { /* the key for "String" */ }
       // etc.
   }
   ```

3. Consider also pre-interning other common names:
   - Keywords: `let`, `fn`, `trait`, `behavior`, `typeof`, etc.
   - Common identifiers: `self`, `Self`, `main`.
   - This depends on how the interner is used elsewhere.

### Implementation steps

1. Add the intern calls to `Default::default()`.
2. Optionally store the returned keys for constant-time access.
3. Verify no tests depend on specific key values (they shouldn't).

### Dependencies

None — this is a standalone change that can be done at any time.

---

## Implementation Order

1. **TODO 5** — Pre-intern builtin types. Trivial, no dependencies.
2. **TODO 1** — Error deduplication. Implement after the error reporting plan.
3. **TODO 3** — Fix stdlib tests. Unblocks test infrastructure.
4. **TODO 2** — Warning failure policy. After stdlib is clean and warnings exist.
5. **TODO 4** — Pipeline integration. After type checker is stable.
