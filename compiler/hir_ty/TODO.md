# Outstanding TODOs for Type Checking

Plans for each category are in the corresponding `PLAN_*.md` files in this directory.

## Generic Type Handling ([plan](PLAN_generic_type_handling.md))
- `src/hir_ty/type_annotation_check.rs:72` — Track generic type variable assignments to ensure consistency
- `src/hir_ty/type_annotation_check.rs:83` — Track that generics have constraints
- `src/hir_ty/hm/inference.rs:283` — Track constraints and enforce them during solving
- `src/hir_ty/hm/inference.rs:317` — Create `ConstrainedGeneric` when a type variable has trait constraints

## Operator Constraint Generation ([plan](PLAN_operator_constraints.md))
- `src/hir_ty/hm/constraint_gen/expr.rs:204` — Generate appropriate constraints for binary operators (arithmetic → numeric, comparison → comparable, etc.)
- `src/hir_ty/hm/constraint_gen/expr.rs:260` — Same for unary operators

## Pattern & Constructor Types ([plan](PLAN_pattern_constructor_types.md))
- `src/hir_ty/hm/inference.rs:210` — Patterns cannot have type annotations, but pattern types can be specified by annotations on expressions
- `src/hir_ty/hm/constraint_gen/pattern.rs:75` — Look up constructor type scheme from target and scope

## Error Reporting ([plan](PLAN_error_reporting.md))
- `src/diagnostics.rs:228` — Duplicate error reporting: both `ConflictingTypeAnnotation` and `UnificationError` fire for the same issue; need deduplication strategy
- `src/diagnostics.rs:274` — Bare `TODO` (unimplemented section)
- `src/hir_ty/hm/mod.rs:321` — Report an error when a reference can't be found by name

## Type Annotation Checking ([plan](PLAN_type_annotation_checking.md))
- `src/hir_ty/type_annotation_check.rs:167` — Implement constraint checking for built-in types, lambdas, etc.

## Bounded Types & Traits ([plan](PLAN_bounded_types_and_traits.md))
- `../hir_resolved/src/type_definition.rs:86` — Bounded type reference should check args
- `../hir_resolved/src/expr.rs:195` — Function calls to abstract trait members

## Infrastructure / Misc ([plan](PLAN_infrastructure.md))
- `src/lib.rs:76` — Add error deduplication if needed
- `src/tests.rs:157,176` — Decide if tests should fail on warnings
- `src/tests.rs:195` — Continue fixing lowering errors in std lib
- `../../compiler/src/lib.rs:69-70` — Phase 3 (type checking) and Phase 4 (IR generation) integration
- `../interner/src/lib.rs:6` — Add builtin types to the interner
