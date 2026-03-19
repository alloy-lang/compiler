//! Hindley-Milner type inference system
//!
//! This module implements a standard HM type inference algorithm with:
//! - Type variables and unification
//! - Let-polymorphism (generalization and instantiation)
//! - Type equations and constraint solving
//! - Support for type class constraints (infrastructure, not yet enforced)

use super::Fql;
use alloy_hir_def as hir;
use alloy_hir_resolved::{EPFql, EPTdFql, HirResolutionError};
use rustc_hash::{FxHashMap, FxHashSet};

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

impl std::fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

/// Monomorphic types (no quantification)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Unconstrained,
    /// Type variable (e.g., `a`, `b`)
    Var(TypeVarId),
    /// Built-in concrete type (e.g., `Int`, `String`)
    Concrete(hir::BuiltInType),
    /// User-defined type constructor (e.g., `List`, `Option`, `MyType`)
    /// The String is the human-readable type name for display purposes
    TypeDef {
        fql: Fql<hir::TypeDefinition>,
        type_args: Vec<TypeVarId>,
        type_def_name: hir::Name,
    },
    /// Function type (e.g., `a -> b`)
    Function(Box<MonoType>, Box<MonoType>),
    /// Tuple type (e.g., `(a, b, c)`)
    Tuple(Vec<MonoType>),
    /// Type application (e.g., `List[Int]`, `Option[String]`)
    /// The constructor is typically a TypeDef, but can be any MonoType
    App {
        constructor: Box<MonoType>,
        args: Vec<MonoType>,
    },
    /// Unit type
    Unit,
}

impl std::fmt::Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonoType::Unconstrained => write!(f, "_"),
            MonoType::Var(var) => write!(f, "t{}", var.0),
            MonoType::Concrete(builtin) => write!(f, "{builtin:?}"),
            MonoType::TypeDef { type_def_name, .. } => {
                write!(f, "{type_def_name}")
            }
            MonoType::Function(arg, ret) => {
                // Add parentheses if arg is also a function
                match arg.as_ref() {
                    MonoType::Function(_, _) => write!(f, "({arg}) -> {ret}"),
                    _ => write!(f, "{arg} -> {ret}"),
                }
            }
            MonoType::Tuple(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            MonoType::App { constructor, args } => {
                write!(f, "{}", constructor)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            MonoType::Unit => write!(f, "()"),
        }
    }
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
    pub(super) source: EPFql,
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
    let mut vars = FxHashSet::default();
    collect_free_vars(ty, &mut vars);
    vars.into_iter().collect()
}

fn collect_free_vars(ty: &MonoType, vars: &mut FxHashSet<TypeVarId>) {
    match ty {
        MonoType::Unconstrained => {}
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
        MonoType::App { constructor, args } => {
            collect_free_vars(constructor, vars);
            for t in args {
                collect_free_vars(t, vars);
            }
        }
        MonoType::Concrete(_) | MonoType::TypeDef { .. } | MonoType::Unit => {}
    }
}

impl PolyType {
    /// Generalize a monotype into a polytype by quantifying free variables
    /// that are not present in the environment
    pub(super) fn generalize(ty: MonoType, env_vars: &FxHashSet<TypeVarId>) -> Self {
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

    pub(super) fn generalize_all(ty: MonoType) -> Self {
        let quantified = free_type_vars(&ty);

        Self {
            quantified,
            constraints: Vec::new(),
            body: ty,
        }
    }

    /// Instantiate a polytype with fresh type variables
    /// Returns (instantiated_type, fresh_vars_in_order)
    /// The fresh_vars Vec contains the fresh variables in the same order as self.quantified
    pub(super) fn instantiate(&self, gen: &mut TypeVarGenerator) -> (MonoType, Vec<TypeVarId>) {
        if self.quantified.is_empty() {
            return (self.body.clone(), Vec::new());
        }

        let mut subst = unification::Substitution::new();
        let mut fresh_vars = Vec::with_capacity(self.quantified.len());

        for var in &self.quantified {
            let fresh = gen.fresh();
            fresh_vars.push(fresh);
            subst.insert(*var, MonoType::Var(fresh));
        }

        (subst.apply(&self.body), fresh_vars)
    }
}

/// Inference context for HM type inference
pub(super) struct HMInferenceContext<'db> {
    pub(super) db: &'db dyn crate::HirInferDatabase,
    /// The module currently being type-checked
    pub(super) module_id: alloy_workspace::ModuleId,
    /// Type variable generator
    pub(super) type_var_gen: TypeVarGenerator,
    /// Type equations to be solved
    pub(super) equations: Vec<TypeEquation>,
    /// Type environment (maps expressions/patterns to their types)
    pub(super) type_env: FxHashMap<EPTdFql, MonoType>,
    /// Polymorphic type schemes for let-bound variables
    pub(super) poly_env: FxHashMap<EPTdFql, PolyType>,
    /// Resolution errors collected during inference (FQL + reference path + module_id)
    pub(super) resolution_errors: Vec<HirResolutionError>,
    /// Map from expression ID to its dependency group index (for lazy constraint generation)
    pub(super) expr_to_group: FxHashMap<hir::ExpressionIdx, usize>,
    /// Current dependency group being processed (for lazy constraint generation)
    pub(super) current_group: Option<usize>,
    /// Type variables that were active before the current group started
    /// (used for proper generalization in let-polymorphism)
    pub(super) env_type_vars: FxHashSet<TypeVarId>,
    /// Maps annotation type variable Fql → TypeVarId
    /// Ensures the same type variable declaration always maps to the same inference variable
    pub(super) annotation_type_vars: FxHashMap<Fql<hir::TypeVariable>, TypeVarId>,
    /// Maps Self type Fql → TypeVarId
    /// Ensures the same Self type in the same trait always maps to the same inference variable
    pub(super) self_type_vars: FxHashMap<Fql<hir::Trait>, TypeVarId>,
    /// Maps TypeVarId → user-visible name (for error messages)
    pub(super) type_var_names: FxHashMap<TypeVarId, hir::Name>,
}

impl<'db> HMInferenceContext<'db> {
    pub(super) fn new(
        db: &'db dyn crate::HirInferDatabase,
        module_id: alloy_workspace::ModuleId,
    ) -> Self {
        Self {
            db,
            module_id,
            type_var_gen: TypeVarGenerator::new(),
            equations: Vec::new(),
            type_env: FxHashMap::default(),
            poly_env: FxHashMap::default(),
            resolution_errors: Vec::new(),
            expr_to_group: FxHashMap::default(),
            current_group: None,
            env_type_vars: FxHashSet::default(),
            annotation_type_vars: FxHashMap::default(),
            self_type_vars: FxHashMap::default(),
            type_var_names: FxHashMap::default(),
        }
    }

    /// Check if an expression is in a later dependency group than the current one
    /// Returns true if we should avoid inferring this expression now
    pub(super) fn is_in_later_group(&self, expr_id: hir::ExpressionIdx) -> bool {
        if let (Some(current), Some(&expr_group)) =
            (self.current_group, self.expr_to_group.get(&expr_id))
        {
            expr_group > current
        } else {
            false
        }
    }

    fn maybe_find_type(&mut self, fql: impl Into<EPTdFql>) -> Option<MonoType> {
        let fql = fql.into();
        if let Some(poly_ty) = self.poly_env.get(&fql).cloned() {
            let (instantiated, _fresh_vars) = poly_ty.instantiate(&mut self.type_var_gen);
            Some(instantiated)
        } else {
            self.type_env.get(&fql).cloned()
        }
    }

    /// Generate a fresh type variable
    pub(super) fn fresh_type_var(&mut self) -> MonoType {
        MonoType::Var(self.type_var_gen.fresh())
    }

    pub(super) fn unknown_reference(
        &mut self,
        err: HirResolutionError,
        fql: impl Into<EPFql>,
    ) -> MonoType {
        self.resolution_errors.push(err);
        let ty = self.fresh_type_var();
        self.assign_type(fql.into(), ty)
    }

    #[must_use]
    fn assign_type(&mut self, fql: impl Into<EPTdFql>, ty: MonoType) -> MonoType {
        self.type_env.insert(fql.into(), ty.clone());
        ty
    }

    /// Add a type equation
    pub(super) fn add_equation(&mut self, left: MonoType, right: MonoType, fql: impl Into<EPFql>) {
        self.equations.push(TypeEquation {
            left,
            right,
            source: fql.into(),
        });
    }

    /// Generalize a type for let-binding
    pub(super) fn generalize_type(&self, ty: MonoType) -> PolyType {
        // Use the environment type variables from before this group
        // This ensures we quantify over type variables local to this expression
        PolyType::generalize(ty, &self.env_type_vars)
    }

    /// Get or create a type variable for an annotation type variable (typevar declaration).
    /// Ensures the same Fql<TypeDefinition> always maps to the same TypeVarId.
    pub(super) fn get_or_create_annotation_type_var(
        &mut self,
        fql: Fql<hir::TypeVariable>,
        name: hir::Name,
    ) -> TypeVarId {
        if let Some(&var_id) = self.annotation_type_vars.get(&fql) {
            return var_id;
        }
        let var_id = self.type_var_gen.fresh();
        self.annotation_type_vars.insert(fql, var_id);
        self.type_var_names.insert(var_id, name);
        var_id
    }

    /// Get or create a type variable for a Self type in a trait context.
    /// Ensures the same Fql<Trait> always maps to the same TypeVarId.
    pub(super) fn get_or_create_self_type_var(&mut self, trait_fql: Fql<hir::Trait>) -> TypeVarId {
        if let Some(&var_id) = self.self_type_vars.get(&trait_fql) {
            return var_id;
        }
        let var_id = self.type_var_gen.fresh();
        self.self_type_vars.insert(trait_fql, var_id);
        self.type_var_names.insert(var_id, hir::Name::new("Self"));
        var_id
    }
}
