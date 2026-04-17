//! Pattern exhaustiveness and unreachability checking for `match` expressions.
//!
//! Current scope (intentionally basic):
//! - Bool scrutinee: require both `True` and `False`, or a wildcard/binding arm.
//! - Enum (union `typedef`) scrutinee: require every variant, or a wildcard arm.
//! - Other scrutinee types (`Int`, `String`, `Char`, generics, lambdas, tuples, …):
//!   require a wildcard/binding arm.
//! - Arms after a catch-all are flagged as unreachable.
//! - Duplicate `Bool` literals and duplicate variants are flagged as unreachable.
//!
//! Deferred — handled by Maranget's algorithm in a future pass:
//! - Nested destructuring exhaustiveness (e.g., `Pair(True, True)` + `Pair(False, _)`).
//! - Literal-set coverage for integrals/strings/chars beyond duplicate detection.
//! - Tuple-by-tuple decomposition.
//!
//! All logic is contained in this module so it can be swapped out for a full
//! matrix-based checker without touching callers.

use crate::diagnostics::{MissingPattern, TypeCheckingErrorKind, UnreachableReason};
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir_def as hir;
use alloy_hir_infer::InferredType;
use alloy_hir_resolved as res;
use alloy_workspace::ModuleId;

pub(crate) fn validate_exhaustiveness(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);

    for (expr_idx, expr, _range, _name) in hir_module.expressions() {
        let hir::Expression::Match { condition, targets } = expr else {
            continue;
        };
        let Some(scrutinee_ty) = result.expression_types.get(condition).cloned() else {
            continue;
        };
        check_match(db, module_id, result, &scrutinee_ty, targets, expr_idx);
    }
}

fn check_match(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
    scrutinee_ty: &InferredType,
    targets: &[(hir::PatternIdx, hir::ExpressionIdx)],
    match_expr_idx: hir::ExpressionIdx,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let mut tracker = CoverageTracker::new(db, module_id, scrutinee_ty);

    for (pattern_idx, _body_idx) in targets {
        let pattern_range = hir_module.get_pattern_range(*pattern_idx);
        let resolved = res::resolve_pattern_by_id(db, module_id, *pattern_idx);

        // Skip if resolution failed — another phase reports that.
        let Ok(resolved_pattern) = resolved else {
            tracker.mark_unknown();
            continue;
        };

        if let Some(reason) = tracker.observe(db, &resolved_pattern) {
            result.error(
                TypeCheckingErrorKind::UnreachablePattern { reason },
                pattern_range,
            );
        }
    }

    let missing = tracker.missing();
    if !missing.is_empty() {
        let match_range = hir_module.get_expression_range(match_expr_idx);
        result.error(
            TypeCheckingErrorKind::NonExhaustiveMatch {
                scrutinee_type: scrutinee_ty.clone(),
                missing,
            },
            match_range,
        );
    }
}

/// Kind of exhaustiveness check we can do for a given scrutinee type.
enum ScrutineeKind {
    Bool,
    /// Nominal type with a finite variant set. If empty, coverage rule is "no arms needed"
    /// but practically unreachable — we still require a wildcard to be safe.
    Enum {
        variants: Vec<hir::Name>,
    },
    /// Single-constructor nominal types (structs, tuples via `typedef Foo = Foo(..)`).
    /// A `DataDestructure` or wildcard covers it.
    SingleVariant,
    /// Tuple, unit, primitive (Int/String/Char/Fraction), lambda, generic, etc.
    /// Only a wildcard/binding arm can exhaust.
    Opaque,
    /// Unknown / unresolved type — skip checks.
    Unknown,
}

/// Walks the arms of a single match, tracking which constructors have been covered.
struct CoverageTracker {
    kind: ScrutineeKind,
    /// Coverage state for `Bool`.
    seen_true: bool,
    seen_false: bool,
    /// Coverage state for `Enum`.
    seen_variants: Vec<hir::Name>,
    /// Coverage state for single-variant / wildcard catch-alls.
    saw_wildcard: bool,
    /// Literal arms already seen (used for duplicate-literal detection on opaque types).
    seen_literals: Vec<hir::Literal>,
}

impl CoverageTracker {
    fn new(db: &dyn HirTyDatabase, module_id: ModuleId, scrutinee_ty: &InferredType) -> Self {
        let kind = classify_scrutinee(db, module_id, scrutinee_ty);
        Self {
            kind,
            seen_true: false,
            seen_false: false,
            seen_variants: Vec::new(),
            saw_wildcard: false,
            seen_literals: Vec::new(),
        }
    }

    /// Called when pattern resolution fails — treat conservatively as a catch-all
    /// so we don't spuriously complain about non-exhaustiveness on top of a real
    /// resolution error.
    fn mark_unknown(&mut self) {
        self.saw_wildcard = true;
    }

    /// Record observing an arm. Returns `Some(reason)` if the arm is unreachable.
    fn observe(&mut self, db: &dyn HirTyDatabase, pattern: &res::Pattern) -> Option<UnreachableReason> {
        if self.saw_wildcard {
            return Some(UnreachableReason::AfterWildcard);
        }
        if self.is_covered() {
            return Some(UnreachableReason::AlreadyExhaustive);
        }

        // Tuple / data-destructure / unit arms are catch-alls only when every
        // nested sub-pattern is itself a catch-all. Otherwise we can't reason
        // about their coverage without Maranget, so we silently accept the arm
        // — neither advancing coverage nor flagging unreachability.
        if matches!(
            pattern,
            res::Pattern::DataDestructure { .. }
                | res::Pattern::Unit
                | res::Pattern::Tuple(_)
        ) {
            if is_catch_all(db, pattern) {
                self.saw_wildcard = true;
            }
            return None;
        }

        match pattern {
            res::Pattern::Missing => None,

            res::Pattern::VariableDeclaration | res::Pattern::Nil => {
                self.saw_wildcard = true;
                None
            }

            res::Pattern::Literal(hir::Literal::Bool(b)) => {
                let already = if *b { self.seen_true } else { self.seen_false };
                if already {
                    Some(UnreachableReason::DuplicateArm)
                } else {
                    if *b {
                        self.seen_true = true;
                    } else {
                        self.seen_false = true;
                    }
                    None
                }
            }

            res::Pattern::Literal(lit) => {
                if self.seen_literals.iter().any(|prev| prev == lit) {
                    Some(UnreachableReason::DuplicateArm)
                } else {
                    self.seen_literals.push(lit.clone());
                    None
                }
            }

            res::Pattern::VariantDestructure { variant_name, args, .. } => {
                if self.seen_variants.iter().any(|v| v == variant_name) {
                    return Some(UnreachableReason::DuplicateArm);
                }
                // Only credit variant coverage when the payload is itself a
                // catch-all. `Some(True)` doesn't exhaust `Some(_)`.
                let all_args_catch_all = args.iter().all(|arg_fql| {
                    match res::resolve_pattern_by_id(db, arg_fql.module_id, arg_fql.local_id) {
                        Ok(p) => is_catch_all(db, &p),
                        Err(_) => true,
                    }
                });
                if all_args_catch_all {
                    self.seen_variants.push(variant_name.clone());
                }
                None
            }

            res::Pattern::DataDestructure { .. }
            | res::Pattern::Unit
            | res::Pattern::Tuple(_) => unreachable!("handled above"),
        }
    }

    fn is_covered(&self) -> bool {
        if self.saw_wildcard {
            return true;
        }
        match &self.kind {
            ScrutineeKind::Bool => self.seen_true && self.seen_false,
            ScrutineeKind::Enum { variants } => variants
                .iter()
                .all(|v| self.seen_variants.iter().any(|seen| seen == v)),
            // Without a wildcard, single-variant and opaque types are never exhausted
            // purely from literal/variant arms. Same for unknown/unsupported
            // scrutinee shapes — we neither declare exhaustion nor report missing.
            ScrutineeKind::SingleVariant | ScrutineeKind::Opaque | ScrutineeKind::Unknown => false,
        }
    }

    fn missing(&self) -> Vec<MissingPattern> {
        if self.is_covered() {
            return Vec::new();
        }
        match &self.kind {
            ScrutineeKind::Bool => {
                let mut out = Vec::new();
                if !self.seen_true {
                    out.push(MissingPattern::BoolLiteral(true));
                }
                if !self.seen_false {
                    out.push(MissingPattern::BoolLiteral(false));
                }
                out
            }
            ScrutineeKind::Enum { variants } => variants
                .iter()
                .filter(|v| !self.seen_variants.iter().any(|seen| &seen == v))
                .cloned()
                .map(MissingPattern::Variant)
                .collect(),
            ScrutineeKind::SingleVariant | ScrutineeKind::Opaque => {
                vec![MissingPattern::Wildcard]
            }
            // Skip reporting — we don't understand the domain well enough to
            // claim it's non-exhaustive.
            ScrutineeKind::Unknown => Vec::new(),
        }
    }
}

fn classify_scrutinee(
    db: &dyn HirTyDatabase,
    _module_id: ModuleId,
    ty: &InferredType,
) -> ScrutineeKind {
    // Peel a Bounded wrapper to get at the underlying TypeDef.
    let inner = match ty {
        InferredType::Bounded { base, .. } => base.as_ref(),
        other => other,
    };

    match inner {
        InferredType::BuiltIn(hir::BuiltInType::Bool) => ScrutineeKind::Bool,
        InferredType::TypeDef(fql, _) => classify_type_def(db, fql),
        InferredType::Missing | InferredType::Unconstrained => ScrutineeKind::Unknown,
        // Tuples need Maranget-style column decomposition to check exhaustively.
        // Until that lands, skip tuple scrutinees entirely.
        InferredType::Tuple(_) => ScrutineeKind::Unknown,
        _ => ScrutineeKind::Opaque,
    }
}

/// A catch-all pattern — one that matches every value of its expected type
/// without inspecting its contents. Variable bindings, wildcards, and compound
/// patterns built entirely out of catch-alls all qualify.
fn is_catch_all(db: &dyn HirTyDatabase, pattern: &res::Pattern) -> bool {
    match pattern {
        res::Pattern::VariableDeclaration | res::Pattern::Nil | res::Pattern::Unit => true,
        res::Pattern::Missing | res::Pattern::Literal(_) | res::Pattern::VariantDestructure { .. } => {
            false
        }
        res::Pattern::Tuple(elements) => elements.iter().all(|fql| {
            match res::resolve_pattern_by_id(db, fql.module_id, fql.local_id) {
                Ok(p) => is_catch_all(db, &p),
                Err(_) => true,
            }
        }),
        res::Pattern::DataDestructure { args, .. } => args.iter().all(|fql| {
            match res::resolve_pattern_by_id(db, fql.module_id, fql.local_id) {
                Ok(p) => is_catch_all(db, &p),
                Err(_) => true,
            }
        }),
    }
}

fn classify_type_def(
    db: &dyn HirTyDatabase,
    fql: &res::Fql<hir::TypeDefinition>,
) -> ScrutineeKind {
    let Some(def) = res::resolve_type_definition_by_id(db, fql.module_id, fql.local_id) else {
        return ScrutineeKind::Unknown;
    };
    match def.kind {
        res::TypeDefinitionKind::Single(_) => ScrutineeKind::SingleVariant,
        res::TypeDefinitionKind::Union(members) => ScrutineeKind::Enum {
            variants: members.iter().map(|m| m.name().clone()).collect(),
        },
    }
}
