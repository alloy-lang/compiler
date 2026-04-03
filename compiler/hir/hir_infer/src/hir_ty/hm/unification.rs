//! Type unification algorithm with occurs check

use super::{EPFql, MonoType, TypeEquation, TypeVarId};
use crate::diagnostics::TypeInferenceError;
use crate::{diagnostics, HirInferDatabase};
use alloy_hir_def as hir;
use alloy_hir_resolved::{resolve_behavior_by_id, Fql};
use diagnostics::TypeInferenceErrorKind;
use rustc_hash::FxHashMap;

/// Substitution mapping type variables to types
#[derive(Debug, Clone)]
pub struct Substitution {
    pub(super) map: FxHashMap<TypeVarId, MonoType>,
}

impl Substitution {
    pub(super) fn new() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }

    pub(super) fn insert(&mut self, var: TypeVarId, ty: MonoType) {
        self.map.insert(var, ty);
    }

    fn get(&self, var: TypeVarId) -> Option<&MonoType> {
        self.map.get(&var)
    }

    pub(super) fn apply(&self, ty: &MonoType) -> MonoType {
        match ty {
            MonoType::Unconstrained => MonoType::Unconstrained,
            MonoType::Var(v) => {
                if let Some(substituted) = self.get(*v) {
                    // Recursively apply in case the substitution contains more variables
                    self.apply(substituted)
                } else {
                    ty.clone()
                }
            }
            MonoType::ConstrainedVar(v, constraints) => {
                if let Some(substituted) = self.get(*v) {
                    let resolved = self.apply(substituted);
                    match resolved {
                        // Propagate constraints through var chains
                        MonoType::Var(v2) => {
                            MonoType::ConstrainedVar(v2, constraints.clone())
                        }
                        // Merge constraints when chaining through another constrained var
                        MonoType::ConstrainedVar(v2, c2) => {
                            let mut merged = constraints.clone();
                            for c in c2 {
                                if !merged.contains(&c) {
                                    merged.push(c);
                                }
                            }
                            MonoType::ConstrainedVar(v2, merged)
                        }
                        // Resolved to concrete type — constraints checked separately
                        other => other,
                    }
                } else {
                    ty.clone()
                }
            }
            MonoType::Function(arg, ret) => {
                MonoType::Function(Box::new(self.apply(arg)), Box::new(self.apply(ret)))
            }
            MonoType::Tuple(tys) => MonoType::Tuple(tys.iter().map(|t| self.apply(t)).collect()),
            MonoType::App { constructor, args } => MonoType::App {
                constructor: Box::new(self.apply(constructor)),
                args: args.iter().map(|t| self.apply(t)).collect(),
            },
            MonoType::TypeDef {
                fql,
                type_args,
                type_def_name,
            } => MonoType::TypeDef {
                fql: fql.clone(),
                type_def_name: type_def_name.clone(),
                type_args: type_args.iter().map(|t| self.apply_type_var(*t)).collect(),
            },
            MonoType::Concrete(_) | MonoType::Unit => ty.clone(),
        }
    }

    pub(super) fn apply_type_var(&self, var: TypeVarId) -> TypeVarId {
        if let Some(substituted) = self.get(var) {
            match self.apply(substituted) {
                MonoType::Var(inner_var) | MonoType::ConstrainedVar(inner_var, _) => {
                    return inner_var;
                }
                _ => {}
            }
        }

        var
    }

    /// Compose two substitutions
    fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::new();

        // Apply `self` to all bindings in `other`
        for (var, ty) in &other.map {
            result.insert(*var, self.apply(ty));
        }

        // Add all bindings from `self` that aren't in `other`
        for (var, ty) in &self.map {
            if !result.map.contains_key(var) {
                result.insert(*var, ty.clone());
            }
        }

        result
    }
}

/// Unification algorithm with occurs check
fn unify_types(t1: &MonoType, t2: &MonoType) -> (Substitution, Vec<UnificationError>) {
    match (t1, t2) {
        // Same type variable
        (MonoType::Unconstrained, _) | (_, MonoType::Unconstrained) => {
            (Substitution::new(), Vec::new())
        }
        (MonoType::Var(v1), MonoType::Var(v2)) if v1 == v2 => (Substitution::new(), Vec::new()),

        // Same variable with constraints on either/both sides
        (MonoType::Var(v1), MonoType::ConstrainedVar(v2, _))
        | (MonoType::ConstrainedVar(v1, _), MonoType::Var(v2))
        | (MonoType::ConstrainedVar(v1, _), MonoType::ConstrainedVar(v2, _))
            if v1 == v2 =>
        {
            (Substitution::new(), Vec::new())
        }

        // Bind type variable to type (covers both Var and ConstrainedVar)
        (MonoType::Var(v), t)
        | (t, MonoType::Var(v))
        | (MonoType::ConstrainedVar(v, _), t)
        | (t, MonoType::ConstrainedVar(v, _)) => {
            if occurs(*v, t) {
                (
                    Substitution::new(),
                    vec![UnificationError::OccursCheck(*v, t.clone())],
                )
            } else {
                let mut subst = Substitution::new();
                subst.insert(*v, t.clone());
                (subst, Vec::new())
            }
        }

        // Function types
        (MonoType::Function(arg1, ret1), MonoType::Function(arg2, ret2)) => {
            let (subst1, err1) = unify_types(arg1, arg2);
            let ret1_subst = subst1.apply(ret1);
            let ret2_subst = subst1.apply(ret2);
            let (subst2, err2) = unify_types(&ret1_subst, &ret2_subst);
            (subst1.compose(&subst2), [err1, err2].concat())
        }

        // Tuple types
        (MonoType::Tuple(ts1), MonoType::Tuple(ts2)) if ts1.len() == ts2.len() => {
            let (subst, mut errors) = unify_many(ts1, ts2);
            if !errors.is_empty() {
                // override member errors with the applied types for better error messages
                errors = vec![UnificationError::TypeMismatch(
                    Box::new(subst.apply(t1)),
                    Box::new(subst.apply(t2)),
                )];
            }
            (subst, errors)
        }

        // Type constructor application
        (
            MonoType::App {
                constructor: c1,
                args: args1,
            },
            MonoType::App {
                constructor: c2,
                args: args2,
            },
        ) if args1.len() == args2.len() => {
            // First unify the constructors
            let (constructor_subst, errors) = unify_types(c1, c2);
            let (args_subst, mut args_errors) = unify_many(args1, args2);
            let subst = constructor_subst.compose(&args_subst);

            if !args_errors.is_empty() {
                // override argument errors with the applied types for better error messages
                args_errors = vec![UnificationError::TypeMismatch(
                    Box::new(subst.apply(t1)),
                    Box::new(subst.apply(t2)),
                )];
            }
            (subst, [errors, args_errors].concat())
        }

        // Type definitions
        (MonoType::TypeDef { fql: fql1, .. }, MonoType::TypeDef { fql: fql2, .. })
            if fql1 == fql2 =>
        {
            (Substitution::new(), Vec::new())
        }

        // Concrete types
        (MonoType::Concrete(c1), MonoType::Concrete(c2)) if c1 == c2 => {
            (Substitution::new(), Vec::new())
        }

        // Unit types
        (MonoType::Unit, MonoType::Unit) => (Substitution::new(), Vec::new()),

        // Mismatch
        _ => (
            Substitution::new(),
            vec![UnificationError::TypeMismatch(
                Box::new(t1.clone()),
                Box::new(t2.clone()),
            )],
        ),
    }
}

fn unify_many(ts1: &[MonoType], ts2: &[MonoType]) -> (Substitution, Vec<UnificationError>) {
    let mut subst = Substitution::new();
    let mut errors = Vec::new();

    for (t1, t2) in ts1.iter().zip(ts2.iter()) {
        let t1_subst = subst.apply(t1);
        let t2_subst = subst.apply(t2);
        let (new_subst, sub_errors) = unify_types(&t1_subst, &t2_subst);
        subst = subst.compose(&new_subst);
        errors.extend(sub_errors);
    }

    (subst, errors)
}

/// Check if a type variable occurs in a type (prevents infinite types)
fn occurs(var: TypeVarId, ty: &MonoType) -> bool {
    match ty {
        MonoType::Unconstrained => false,
        MonoType::Var(v) | MonoType::ConstrainedVar(v, _) => *v == var,
        MonoType::Function(arg, ret) => occurs(var, arg) || occurs(var, ret),
        MonoType::Tuple(tys) => tys.iter().any(|t| occurs(var, t)),
        MonoType::App { constructor, args } => {
            occurs(var, constructor) || args.iter().any(|t| occurs(var, t))
        }
        MonoType::Concrete(_) | MonoType::TypeDef { .. } | MonoType::Unit => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnificationError {
    /// Occurs check failed (would create infinite type)
    OccursCheck(TypeVarId, MonoType),
    TypeMismatch(Box<MonoType>, Box<MonoType>),
}

/// Solve a list of type equations and check trait constraints.
///
/// Runs unification in two passes:
/// 1. Collect substitutions (ignore errors)
/// 2. Re-apply substitutions and collect unification errors
///
/// After the first pass, collects constraints from `ConstrainedVar` nodes in
/// the equation types and checks that resolved concrete types satisfy their
/// trait constraints.
///
/// Returns the substitution, a constraint map (for output conversion), and errors.
pub(super) fn solve_equations(
    db: &dyn HirInferDatabase,
    equations: Vec<TypeEquation>,
) -> (
    Substitution,
    FxHashMap<TypeVarId, Vec<(Fql<hir::Trait>, hir::Name)>>,
    Vec<TypeInferenceError>,
) {
    let mut subst = Substitution::new();
    let mut errors = Vec::new();

    // First pass: collect substitutions
    for equation in &equations {
        let left = subst.apply(&equation.left);
        let right = subst.apply(&equation.right);
        let (new_subst, _errors) = unify_types(&left, &right);
        subst = subst.compose(&new_subst);
    }

    // Collect constraints from ConstrainedVar nodes in equation types,
    // resolving through the substitution to find the target variable.
    let constraint_map = collect_constraints(&equations, &subst);

    // Check constraints: when a constrained type variable resolves to a
    // concrete type, verify that the type implements the required traits.
    for (var_id, constraints) in &constraint_map {
        let resolved = subst.apply(&MonoType::Var(*var_id));
        if let MonoType::TypeDef {
            fql: type_fql,
            type_def_name,
            ..
        } = &resolved
        {
            for (trait_fql, trait_name) in constraints {
                if !has_behavior_for_trait(db, type_fql, trait_fql) {
                    // Find the equation that caused this binding for a precise range
                    let range = find_equation_range(db, &equations, *var_id, &subst);
                    errors.push(TypeInferenceError::new(
                        TypeInferenceErrorKind::UnsatisfiedConstraint {
                            trait_name: trait_name.clone(),
                            type_name: type_def_name.clone(),
                        },
                        range,
                    ));
                }
            }
        }
    }

    // Second pass: collect unification errors
    for equation in &equations {
        let left = subst.apply(&equation.left);
        let right = subst.apply(&equation.right);
        let (_, unification_errors) = unify_types(&left, &right);

        for err in unification_errors {
            let (hir_module, _) = hir::lower_file(db, equation.source.module_id());
            let range = match &equation.source {
                EPFql::Expression(fql) => hir_module.get_expression_range(fql.local_id),
                EPFql::Pattern(fql) => hir_module.get_pattern_range(fql.local_id),
            };

            errors.push(TypeInferenceError::new(
                TypeInferenceErrorKind::UnificationError(err),
                range,
            ));
        }
    }

    (subst, constraint_map, errors)
}

/// Walk equation types to collect all ConstrainedVar constraints,
/// resolving variable IDs through the substitution.
fn collect_constraints(
    equations: &[TypeEquation],
    subst: &Substitution,
) -> FxHashMap<TypeVarId, Vec<(Fql<hir::Trait>, hir::Name)>> {
    let mut constraints = FxHashMap::default();
    for equation in equations {
        collect_constrained_vars(&equation.left, subst, &mut constraints);
        collect_constrained_vars(&equation.right, subst, &mut constraints);
    }
    constraints
}

fn collect_constrained_vars(
    ty: &MonoType,
    subst: &Substitution,
    constraints: &mut FxHashMap<TypeVarId, Vec<(Fql<hir::Trait>, hir::Name)>>,
) {
    match ty {
        MonoType::ConstrainedVar(v, c) => {
            let target = subst.apply_type_var(*v);
            let store = constraints.entry(target).or_default();
            for constraint in c {
                if !store.contains(constraint) {
                    store.push(constraint.clone());
                }
            }
        }
        MonoType::Function(arg, ret) => {
            collect_constrained_vars(arg, subst, constraints);
            collect_constrained_vars(ret, subst, constraints);
        }
        MonoType::Tuple(tys) => {
            for t in tys {
                collect_constrained_vars(t, subst, constraints);
            }
        }
        MonoType::App { constructor, args } => {
            collect_constrained_vars(constructor, subst, constraints);
            for a in args {
                collect_constrained_vars(a, subst, constraints);
            }
        }
        MonoType::Unconstrained
        | MonoType::Var(_)
        | MonoType::Concrete(_)
        | MonoType::TypeDef { .. }
        | MonoType::Unit => {}
    }
}

/// Find the range of the equation that caused a constrained variable to
/// resolve to a concrete type, for precise error reporting.
fn find_equation_range(
    db: &dyn HirInferDatabase,
    equations: &[TypeEquation],
    var_id: TypeVarId,
    subst: &Substitution,
) -> text_size::TextRange {
    // Look for an equation where applying the substitution resolves our var
    for equation in equations {
        let mentions_var = mentions_type_var(&equation.left, var_id, subst)
            || mentions_type_var(&equation.right, var_id, subst);
        if mentions_var {
            let (hir_module, _) = hir::lower_file(db, equation.source.module_id());
            return match &equation.source {
                EPFql::Expression(fql) => hir_module.get_expression_range(fql.local_id),
                EPFql::Pattern(fql) => hir_module.get_pattern_range(fql.local_id),
            };
        }
    }
    text_size::TextRange::default()
}

/// Check if a MonoType mentions a type variable (directly or through substitution).
fn mentions_type_var(ty: &MonoType, target: TypeVarId, subst: &Substitution) -> bool {
    match ty {
        MonoType::Var(v) | MonoType::ConstrainedVar(v, _) => {
            *v == target || subst.apply_type_var(*v) == target
        }
        MonoType::Function(arg, ret) => {
            mentions_type_var(arg, target, subst) || mentions_type_var(ret, target, subst)
        }
        MonoType::Tuple(tys) => tys.iter().any(|t| mentions_type_var(t, target, subst)),
        MonoType::App { constructor, args } => {
            mentions_type_var(constructor, target, subst)
                || args.iter().any(|t| mentions_type_var(t, target, subst))
        }
        MonoType::Unconstrained
        | MonoType::Concrete(_)
        | MonoType::TypeDef { .. }
        | MonoType::Unit => false,
    }
}

/// Check if a type has a behavior implementation for the required trait.
fn has_behavior_for_trait(
    db: &dyn HirInferDatabase,
    type_fql: &Fql<hir::TypeDefinition>,
    required_trait: &Fql<hir::Trait>,
) -> bool {
    let (hir_module, _) = hir::lower_file(db, type_fql.module_id);
    for (behavior_idx, _, _, _) in hir_module.behaviors() {
        let behavior = resolve_behavior_by_id(db, type_fql.module_id, behavior_idx);
        let Ok(attached_type) = &behavior.attached_type else {
            continue;
        };
        let Ok(attached_trait) = &behavior.attached_trait else {
            continue;
        };
        if attached_type == type_fql && attached_trait == required_trait {
            return true;
        }
    }
    false
}
