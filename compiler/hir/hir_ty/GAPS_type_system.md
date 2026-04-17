# Type System Gaps

Audit of what's unfinished in the type system before codegen is a safe next step.
Generated 2026-04-15 on `refactor/hm`.

## Type-checking correctness

- **Unary operator constraints** — `-x` doesn't enforce numeric; accepts strings,
  bools, lambdas. See `PLAN_operator_constraints.md`.
- **Trait constraints on builtins/lambdas/tuples** — `check_trait_constraints`
  falls through with `_ => Ok(())` for everything except user types and generics.
  So `Int : SomeTrait` passes even when no behavior exists.
  See `PLAN_type_annotation_checking.md`.

## Pipeline wiring

- **Compiler integration** — `compile()` in `compiler/src/lib.rs` stops after
  hir_def lowering. Phase 3 (type-checking) isn't wired in, so nothing currently
  invokes the type checker end-to-end; the test harnesses are the only call sites.
  **This is blocking for codegen.** See `PLAN_infrastructure.md` TODO 3.
- **Interner bootstrap** — builtin names not pre-interned. Trivial but easy to
  forget. See `PLAN_infrastructure.md` TODO 4.

## Cosmetic / future

- No warnings exist at all (`TypeInferenceWarningKind` is empty). Unused variables,
  redundant annotations, unreachable patterns — none detected.
- Test harness warning policy is a decision waiting on the above.
- `is_hidden_by` rules are still hand-coded per code; fine for now.

## Not listed in PLANs but worth flagging

### Recursion

Verified 2026-04-16 via `probe_recursion__*.test` snapshots:

- **Direct recursion (annotated)** — `typeof f : Int -> Int` + `let f = |n| -> f(n)` →
  infers `Int -> Int`.
- **Direct recursion (unannotated)** — `let f = |n| -> f(n)` → no crash, infers
  `a0 -> b0` (a valid principal HM type — the body adds no info).
- **Mutual recursion (annotated)** — `is_even`/`is_odd` with full annotations → works.
- **Mutual recursion (unannotated)** — no crash, but inferred types per side are
  garbage (generic IDs don't line up across the Salsa query boundary).

**Fix applied 2026-04-16** in `constraint_gen/expr/lambda.rs`: pre-bind the
lambda's type shape (arg types + fresh return var) in `type_env` *before*
inferring the body, then unify the return var with the body type. This lets
`variable_ref::infer` resolve self-references through `type_env` instead of
re-entering `infer_expr_hm` on the same expression. Fixes direct recursion
fully and any mutual recursion that shares a single `HMInferenceContext`.

Mutual unannotated top-level recursion is the only remaining rough edge. Each
`infer_body_type` runs in its own Salsa query → its own `HMInferenceContext`
→ its own TypeVarId space, so cross-def equations never get set up. This is
expected to become a lowering error upstream (unannotated top-level value
definitions should be rejected at lowering), so we're not pursuing
SCC-grouped inference — it was tried previously and removed as too complex.

Sanity-check companion fix in `inference.rs::infer_body_type_cycle_initial`:
the cycle fallback now pre-populates `expression_types[value_def.expression_idx]`
with `InferredType::Unconstrained`. `value::infer` still indexes directly into
the map (any *other* missing key is a real bug), but the one known-reachable
cycle case is handled explicitly rather than via a broad `unwrap_or` that
could mask unrelated map-population regressions.

### Pattern exhaustiveness — basic checker in place (2026-04-17)

Lives in `validation/exhaustiveness.rs`. Produces two error codes:

- **E34004 NonExhaustiveMatch** — missing patterns.
- **E34005 UnreachablePattern** — arms after a catch-all, duplicate
  literal/variant arms, or arms after full coverage.

Scope checked:
- **Bool** — requires `True` and `False` or a wildcard.
- **Union `typedef`** — requires every variant or a wildcard.
- **Single-variant `typedef` / other (Int, String, Char, lambda, generic)** —
  requires a wildcard/binding arm.
- **Top-level tuple, data-destructure, unit arms** — count as catch-alls
  only when every nested sub-pattern is itself a catch-all (variable
  bindings, `_`, or compound patterns built entirely from catch-alls).

Deferred — intentionally handled by Maranget's algorithm later:
- **Tuple scrutinees** — classified as `Unknown`, so no non-exhaustive or
  unreachable errors are produced. Needed column-wise for correctness
  (e.g. `(Option::None, _)` + `(_, Option::None)` + `(Option::Some(_),
  Option::Some(_))` is exhaustive but my simple checker can't prove it).
- **Nested variant payload decomposition** — `Some(True)` does not
  exhaust `Some(_)`; the checker recognizes this by declining to credit
  the variant, but cannot prove exhaustion over `Some(True) | Some(False)`.
- **Literal-set coverage** for Int/String/Char beyond duplicate detection.

The module is self-contained so Maranget's full matrix decomposition can
replace it without touching callers.

### Trait coherence / orphan rules

Unclear whether duplicate/overlapping behaviors are rejected (E22007 detection
was flagged as buggy in memory). Not re-verified in this pass.

## Overall read

The showstopper for codegen remains the integration gap (Phase 3 wiring).
Basic exhaustiveness is now in place, but tuple scrutinees and nested
patterns still need Maranget's algorithm before codegen can safely skip
default traps.

Suggested order before codegen:

1. ~~Fix direct recursion (pre-bind lambda type var before body inference).~~ **DONE 2026-04-16.**
2. ~~Decide on mutual-unannotated-recursion semantics.~~ Punted to lowering —
   unannotated top-level definitions should be a lowering error; no further
   type-inference work needed here.
3. ~~Stub exhaustiveness at least to the level of "warn on missing variant".~~
   **DONE 2026-04-17** — basic checker covers Bool, enums, catch-all arms,
   and unreachability. Tuples + nested patterns deferred to a future
   Maranget-based pass.
4. Then wire up Phase 3 in `compiler/src/lib.rs`.
