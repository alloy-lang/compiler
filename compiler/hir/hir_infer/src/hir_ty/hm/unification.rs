//! Type unification algorithm with occurs check

use super::{EPFql, MonoType, TypeEquation, TypeVarId};
use crate::diagnostics::TypeInferenceError;
use crate::hir_ty::hm::converter::ToInferredTypeConverter;
use crate::{diagnostics, HirInferDatabase};
use alloy_hir_def as hir;
use alloy_hir_resolved::{EPTdFql, TraitConstraint};
use diagnostics::TypeInferenceErrorKind;
use rustc_hash::{FxHashMap, FxHashSet};

/// Substitution mapping type variables to types
#[derive(Debug, Clone)]
pub struct Substitution {
    map: FxHashMap<TypeVarId, MonoType>,
    constraints: FxHashMap<TypeVarId, Vec<TraitConstraint>>,
    source_fqls: FxHashMap<TypeVarId, EPTdFql>,
}

impl Substitution {
    pub(super) fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            constraints: FxHashMap::default(),
            source_fqls: FxHashMap::default(),
        }
    }

    pub(super) fn insert(&mut self, var: TypeVarId, ty: MonoType, source_fql: &EPTdFql) {
        self.map.insert(var, ty.clone());
        self.source_fqls.insert(var, source_fql.clone());
    }

    fn insert_constraints(&mut self, var: TypeVarId, ty: &MonoType) {
        let target = self.apply_type_var(var);
        if let MonoType::ConstrainedVar(_, ref c) = ty {
            let store = self.constraints.entry(target).or_default();
            for constraint in c {
                if !store
                    .iter()
                    .any(|existing| existing.trait_fql == constraint.trait_fql)
                {
                    store.push(constraint.clone());
                }
            }
        }
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
                        MonoType::Var(v2) => MonoType::ConstrainedVar(v2, constraints.clone()),
                        // Merge constraints when chaining through another constrained var
                        MonoType::ConstrainedVar(v2, c2) => {
                            let mut merged = constraints.clone();
                            for c in c2 {
                                if !merged
                                    .iter()
                                    .any(|existing| existing.trait_fql == c.trait_fql)
                                {
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
            result.insert(*var, self.apply(ty), &other.source_fqls[var]);
        }

        // Add all bindings from `self` that aren't in `other`
        for (var, ty) in &self.map {
            if !result.map.contains_key(var) {
                result.insert(*var, ty.clone(), &self.source_fqls[var]);
            }
        }

        for (var, cs) in &self.constraints {
            result.insert_constraints(*var, &MonoType::ConstrainedVar(*var, cs.clone()));
        }
        for (var, cs) in &other.constraints {
            result.insert_constraints(*var, &MonoType::ConstrainedVar(*var, cs.clone()));
        }

        result
    }
}

/// Unification algorithm with occurs check
fn unify_types(
    actual: &MonoType,
    expected: &MonoType,
    source_fql: &EPTdFql,
) -> (Substitution, Vec<UnificationError>) {
    match (actual, expected) {
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
                subst.insert(*v, t.clone(), source_fql);
                subst.insert_constraints(*v, expected);
                subst.insert_constraints(*v, actual);
                (subst, Vec::new())
            }
        }

        // Function types
        (MonoType::Function(arg1, ret1), MonoType::Function(arg2, ret2)) => {
            let (subst1, err1) = unify_types(arg1, arg2, source_fql);
            let ret1_subst = subst1.apply(ret1);
            let ret2_subst = subst1.apply(ret2);
            let (subst2, err2) = unify_types(&ret1_subst, &ret2_subst, source_fql);
            (subst1.compose(&subst2), [err1, err2].concat())
        }

        // Tuple types
        (MonoType::Tuple(ts1), MonoType::Tuple(ts2)) if ts1.len() == ts2.len() => {
            let (subst, mut errors) = unify_many(ts1, ts2, source_fql);
            if !errors.is_empty() {
                // override member errors with the applied types for better error messages
                errors = vec![UnificationError::TypeMismatch(
                    Box::new(subst.apply(actual)),
                    Box::new(subst.apply(expected)),
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
            let (constructor_subst, errors) = unify_types(c1, c2, source_fql);
            let (args_subst, mut args_errors) = unify_many(args1, args2, source_fql);
            let subst = constructor_subst.compose(&args_subst);

            if !args_errors.is_empty() {
                // override argument errors with the applied types for better error messages
                args_errors = vec![UnificationError::TypeMismatch(
                    Box::new(subst.apply(actual)),
                    Box::new(subst.apply(expected)),
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
                Box::new(actual.clone()),
                Box::new(expected.clone()),
            )],
        ),
    }
}

fn unify_many(
    ts1: &[MonoType],
    ts2: &[MonoType],
    source_fql: &EPTdFql,
) -> (Substitution, Vec<UnificationError>) {
    let mut subst = Substitution::new();
    let mut errors = Vec::new();

    for (t1, t2) in ts1.iter().zip(ts2.iter()) {
        let t1_subst = subst.apply(t1);
        let t2_subst = subst.apply(t2);
        let (new_subst, sub_errors) = unify_types(&t1_subst, &t2_subst, source_fql);
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
    converter: &mut ToInferredTypeConverter,
) -> (Substitution, Vec<TypeInferenceError>) {
    let mut subst = Substitution::new();
    let mut errors = Vec::new();

    // First pass: collect substitutions and constraints.
    // Unify as (actual, expected) so that inferred type variables bind to
    // annotation variables, preserving user-chosen names like `t` over
    // auto-generated names like `a0`.
    for equation in &equations {
        let expected = subst.apply(&equation.expected);
        let actual = subst.apply(&equation.actual);
        let source_fql = equation.source.clone().into();

        let (new_subst, _errors) = unify_types(&actual, &expected, &source_fql);
        subst = subst.compose(&new_subst);
    }

    let constraint_map = subst.constraints.clone();

    // Check constraints: when a constrained type variable resolves to a
    // concrete type, verify that the type implements the required traits.
    for (var_id, constraints) in &constraint_map {
        let resolved = subst.apply(&MonoType::Var(*var_id));
        for constraint in constraints {
            if !resolved.satisfies_constraint(db, constraint) {
                let source_fql = &subst.source_fqls[var_id];
                let range = source_fql.text_range(db);

                let (hir_module, _) =
                    hir::lower_file(db, constraint.type_var_constraint_fql.module_id);

                let constraint_range = hir_module.get_type_variable_constraint_range(
                    constraint.type_var_constraint_fql.local_id,
                );

                let converted = converter.mono_to_inferred(&resolved, &FxHashSet::default());
                errors.push(TypeInferenceError::new(
                    TypeInferenceErrorKind::UnsatisfiedConstraint {
                        trait_fql_name: constraint.trait_fql_name.clone(),
                        resolved_type: converted,
                        constraint_range,
                    },
                    range,
                ))
            }
        }
    }

    // Second pass: collect unification errors
    for equation in &equations {
        let expected = subst.apply(&equation.expected);
        let actual = subst.apply(&equation.actual);
        let source_fql = equation.source.clone().into();

        let (_, unification_errors) = unify_types(&expected, &actual, &source_fql);

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

    (subst, errors)
}
