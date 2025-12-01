//! Hindley-Milner type inference system
//!
//! This module implements a standard HM type inference algorithm with:
//! - Type variables and unification
//! - Let-polymorphism (generalization and instantiation)
//! - Type equations and constraint solving
//! - Support for type class constraints (infrastructure, not yet enforced)

use alloy_hir as hir;
use rustc_hash::FxHashMap;

use super::{ExpressionOrPatternIdx, Fql};

mod constraint_gen;
mod inference;
pub mod unification;

pub use inference::infer_types_hm;

/// Unique identifier for a type variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(usize);

impl TypeVarId {
    fn new(id: usize) -> Self {
        Self(id)
    }
}

/// Monomorphic types (no quantification)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Unconstrained,
    Missing,
    /// Type variable (e.g., `a`, `b`)
    Var(TypeVarId),
    /// Built-in concrete type (e.g., `Int`, `String`)
    Concrete(hir::BuiltInType),
    /// Function type (e.g., `a -> b`)
    Function(Box<MonoType>, Box<MonoType>),
    /// Tuple type (e.g., `(a, b, c)`)
    Tuple(Vec<MonoType>),
    /// Type application (e.g., `List a`, `Option Int`)
    App {
        constructor: Fql<hir::TypeReference>,
        args: Vec<MonoType>,
    },
    /// Unit type
    Unit,
}

/// Polymorphic type scheme (with quantification)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyType {
    /// Quantified type variables
    quantified: Vec<TypeVarId>,
    /// Type class constraints (e.g., `Eq a`, `Ord b`)
    constraints: Vec<TypeConstraint>,
    /// The body type
    body: MonoType,
}

/// A type class constraint (e.g., `Eq a`, `Ord b`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeConstraint {
    /// The trait/type class
    trait_ref: Fql<hir::Trait>,
    /// The type variable it constrains
    type_var: TypeVarId,
}

/// A type equation for unification (e.g., `τ1 = τ2`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEquation {
    pub(super) left: MonoType,
    pub(super) right: MonoType,
    /// Source location for error reporting
    pub(super) source: ExpressionOrPatternIdx,
}

/// Context for generating fresh type variables
pub struct TypeVarGenerator {
    next_id: usize,
}

impl TypeVarGenerator {
    fn new() -> Self {
        Self { next_id: 0 }
    }

    fn fresh(&mut self) -> TypeVarId {
        let id = self.next_id;
        self.next_id += 1;
        TypeVarId::new(id)
    }
}

/// Compute the free type variables in a type
pub(super) fn free_type_vars(ty: &MonoType) -> Vec<TypeVarId> {
    use rustc_hash::FxHashSet;
    let mut vars = FxHashSet::default();
    collect_free_vars(ty, &mut vars);
    vars.into_iter().collect()
}

fn collect_free_vars(ty: &MonoType, vars: &mut rustc_hash::FxHashSet<TypeVarId>) {
    match ty {
        MonoType::Unconstrained => {}
        MonoType::Missing => {}
        MonoType::Var(v) => {
            vars.insert(*v);
        }
        MonoType::Function(arg, ret) => {
            collect_free_vars(arg, vars);
            collect_free_vars(ret, vars);
        }
        MonoType::Tuple(tys) => {
            for t in tys {
                collect_free_vars(t, vars);
            }
        }
        MonoType::App { args, .. } => {
            for t in args {
                collect_free_vars(t, vars);
            }
        }
        MonoType::Concrete(_) | MonoType::Unit => {}
    }
}

impl PolyType {
    /// Generalize a monotype into a polytype by quantifying free variables
    /// that are not present in the environment
    pub(super) fn generalize(ty: MonoType, env_vars: &rustc_hash::FxHashSet<TypeVarId>) -> Self {
        let free_vars = free_type_vars(&ty);
        let quantified: Vec<TypeVarId> = free_vars
            .into_iter()
            .filter(|v| !env_vars.contains(v))
            .collect();

        Self {
            quantified,
            constraints: Vec::new(),
            body: ty,
        }
    }

    /// Instantiate a polytype with fresh type variables
    pub(super) fn instantiate(&self, gen: &mut TypeVarGenerator) -> MonoType {
        if self.quantified.is_empty() {
            return self.body.clone();
        }

        let mut subst = unification::Substitution::new();
        for var in &self.quantified {
            subst.insert(*var, MonoType::Var(gen.fresh()));
        }

        subst.apply(&self.body)
    }
}

/// Inference context for HM type inference
pub(super) struct HMInferenceContext<'db> {
    pub(super) db: &'db dyn crate::HirTyDatabase,
    /// Type variable generator
    pub(super) type_var_gen: TypeVarGenerator,
    /// Type equations to be solved
    pub(super) equations: Vec<TypeEquation>,
    /// Type environment (maps expressions/patterns to their types)
    pub(super) type_env: FxHashMap<ExpressionOrPatternIdx, MonoType>,
    /// Polymorphic type schemes for let-bound variables
    pub(super) poly_env: FxHashMap<ExpressionOrPatternIdx, PolyType>,
}

impl<'db> HMInferenceContext<'db> {
    pub(super) fn new(db: &'db dyn crate::HirTyDatabase) -> Self {
        Self {
            db,
            type_var_gen: TypeVarGenerator::new(),
            equations: Vec::new(),
            type_env: FxHashMap::default(),
            poly_env: FxHashMap::default(),
        }
    }

    /// Generate a fresh type variable
    pub(super) fn fresh_type_var(&mut self) -> MonoType {
        MonoType::Var(self.type_var_gen.fresh())
    }

    /// Assign a type to an expression or pattern
    pub(super) fn assign_type(&mut self, id: ExpressionOrPatternIdx, ty: MonoType) {
        println!("Assigning type {:?} to {:?}", ty, id);
        self.type_env.insert(id, ty);
    }

    /// Add a type equation
    pub(super) fn add_equation(
        &mut self,
        left: MonoType,
        right: MonoType,
        source: ExpressionOrPatternIdx,
    ) {
        println!("Adding equation {:?} = {:?} at {:?}", left, right, source);
        self.equations.push(TypeEquation {
            left,
            right,
            source,
        });
    }

    /// Generalize a type for let-binding
    pub(super) fn generalize_type(&self, ty: MonoType) -> PolyType {
        // Get all free variables in the current environment
        let mut env_vars = rustc_hash::FxHashSet::default();
        for t in self.type_env.values() {
            env_vars.extend(free_type_vars(t));
        }
        PolyType::generalize(ty, &env_vars)
    }

    /// Instantiate a polymorphic type
    pub(super) fn instantiate_poly(&mut self, poly: &PolyType) -> MonoType {
        poly.instantiate(&mut self.type_var_gen)
    }
}
