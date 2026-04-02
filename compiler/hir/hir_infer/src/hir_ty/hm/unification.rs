//! Type unification algorithm with occurs check

use super::{EPFql, MonoType, TypeEquation, TypeVarId};
use crate::diagnostics::TypeInferenceError;
use crate::{diagnostics, HirInferDatabase};
use alloy_hir_def as hir;
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
            if let MonoType::Var(inner_var) = self.apply(substituted) {
                return inner_var;
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

        // Bind type variable to type
        (MonoType::Var(v), t) | (t, MonoType::Var(v)) => {
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
        MonoType::Var(v) => *v == var,
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

/// Solve a list of type equations
pub(super) fn solve_equations(
    db: &dyn HirInferDatabase,
    equations: Vec<TypeEquation>,
) -> (Substitution, Vec<TypeInferenceError>) {
    let mut subst = Substitution::new();
    let mut unification_errors = Vec::new();

    for equation in &equations {
        let left = subst.apply(&equation.left);
        let right = subst.apply(&equation.right);
        let (new_subst, _errors) = unify_types(&left, &right);
        subst = subst.compose(&new_subst);
    }
    for equation in &equations {
        let left = subst.apply(&equation.left);
        let right = subst.apply(&equation.right);
        let (_, errors) = unify_types(&left, &right);

        for err in errors {
            let (hir_module, _) = hir::lower_file(db, equation.source.module_id());
            let range = match &equation.source {
                EPFql::Expression(fql) => hir_module.get_expression_range(fql.local_id),
                EPFql::Pattern(fql) => hir_module.get_pattern_range(fql.local_id),
            };

            unification_errors.push(TypeInferenceError::new(
                TypeInferenceErrorKind::UnificationError(err),
                range,
            ));
        }
    }

    (subst, unification_errors)
}
