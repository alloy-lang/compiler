# Plan: Bounded Types & Traits

## Overview

One remaining TODO in the `hir_resolved` layer: validating type arguments on bounded type references.

Abstract trait member function calls have been implemented — both the resolution layer (`Expression::AbstractTraitFunctionCall`) and the constraint generation layer (`infer_abstract_member_call()`) handle them.

---

## TODO: Bounded type reference should check args

**File:** `compiler/hir/hir_resolved/src/annotated_type.rs` (or the type definition resolution code)

### Problem

When resolving a bounded type reference like `List[Int, String]`, only the base type (`List`) is resolved. The type arguments (`Int, String`) are not validated:
- No check that the base type accepts generic parameters.
- No arity check (number of arguments vs number of parameters).
- No validation that each argument is itself a valid type.

### Plan

1. **Resolve the base type** as before to get the `TypeDefinition`.

2. **Get the expected parameter count** from the type definition.

3. **Check arity:**
   ```rust
   if expected_count != actual_count {
       // Report arity mismatch error
   }
   ```

4. **Validate each argument** by resolving it as a type reference.

5. **Keep the function's return type unchanged** — this function's job is name resolution. Full type checking happens in the inference/validation layers.

6. **Add error reporting** for arity mismatches and invalid arguments.

### Tests

- `Option[Int]` — valid, Option has 1 parameter.
- `Option[Int, String]` — arity error, too many arguments.
- `Int[String]` — error, Int is a built-in with no parameters.
- Nested: `Option[List[Int]]` — should validate recursively.
