//! Hindley-Milner type inference system
//!
//! This module implements a standard HM type inference algorithm with:
//! - Type variables and unification
//! - Let-polymorphism (generalization and instantiation)
//! - Type equations and constraint solving
//! - Support for type class constraints (infrastructure, not yet enforced)

use super::Fql;
use alloy_hir_def as hir;
use alloy_hir_resolved::{
    AnnotatedType, AnnotatedTypeVar, EPFql, EPTdFql, HirResolutionError, TraitConstraint,
    TypeVarReference,
};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

mod constraint_gen;
mod inference;
pub mod unification;

use crate::HirInferDatabase;
pub(crate) use inference::infer_body_type;
pub(crate) use inference::infer_expressions;

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
    /// Type variable with trait constraints (e.g., `a : Eq`).
    /// Carries constraints inline, eliminating the need for a separate constraint store.
    ConstrainedVar(TypeVarId, Vec<TraitConstraint>),
    /// Unit type
    Unit,
}

impl MonoType {
    pub(crate) fn is_polymorphic(&self) -> bool {
        match self {
            MonoType::Var(_) | MonoType::ConstrainedVar(_, _) => true,
            MonoType::Function(arg, ret) => arg.is_polymorphic() || ret.is_polymorphic(),
            MonoType::Tuple(elements) => elements.iter().any(Self::is_polymorphic),
            MonoType::App { constructor, args } => {
                constructor.is_polymorphic() || args.iter().any(Self::is_polymorphic)
            }
            MonoType::Unconstrained
            | MonoType::Concrete(_)
            | MonoType::TypeDef { .. }
            | MonoType::Unit => false,
        }
    }

    pub(crate) fn free_type_vars(&self) -> Vec<TypeVarId> {
        let mut vars = FxHashSet::default();
        self.collect_free_vars(&mut vars);
        vars.into_iter().collect()
    }

    fn collect_free_vars(&self, vars: &mut FxHashSet<TypeVarId>) {
        match self {
            MonoType::Unconstrained => {}
            MonoType::Var(v) | MonoType::ConstrainedVar(v, _) => {
                vars.insert(*v);
            }
            MonoType::Function(arg, ret) => {
                arg.collect_free_vars(vars);
                ret.collect_free_vars(vars);
            }
            MonoType::Tuple(tys) => {
                for t in tys {
                    t.collect_free_vars(vars);
                }
            }
            MonoType::App { constructor, args } => {
                constructor.collect_free_vars(vars);
                for t in args {
                    t.collect_free_vars(vars);
                }
            }
            MonoType::Concrete(_) | MonoType::TypeDef { .. } | MonoType::Unit => {}
        }
    }

    pub fn satisfies_constraint(
        &self,
        db: &dyn HirInferDatabase,
        constraint: &TraitConstraint,
    ) -> bool {
        match self {
            MonoType::TypeDef { fql: type_fql, .. } => {
                constraint.has_behavior_for_trait(db, type_fql)
            }
            MonoType::Tuple(elements) => elements
                .iter()
                .all(|elem| elem.satisfies_constraint(db, constraint)),
            MonoType::App { constructor, args } => {
                constructor.satisfies_constraint(db, constraint)
                    && args
                        .iter()
                        .all(|arg| arg.satisfies_constraint(db, constraint))
            }
            MonoType::Concrete(builtin) => match builtin {
                hir::BuiltInType::Int
                | hir::BuiltInType::Fraction
                | hir::BuiltInType::String
                | hir::BuiltInType::Char => matches!(
                    constraint.trait_fql_name.as_str(),
                    "std::eq::Eq" | "std::order::Ord" | "std::debug::Debug"
                ),
                hir::BuiltInType::Bool => {
                    matches!(
                        constraint.trait_fql_name.as_str(),
                        "std::eq::Eq" | "std::debug::Debug"
                    )
                }
            },
            MonoType::Unit => matches!(
                constraint.trait_fql_name.as_str(),
                "std::eq::Eq" | "std::debug::Debug"
            ),
            MonoType::Function(_, _) => false, // functions cannot implement traits
            MonoType::Unconstrained => true, // unconstrained types are considered to satisfy all constraints
            MonoType::Var(_) => true,
            MonoType::ConstrainedVar(_, constraints) => constraints.contains(constraint),
        }
    }
}

impl std::fmt::Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonoType::Unconstrained => write!(f, "_"),
            MonoType::Var(var) => write!(f, "t{}", var.0),
            MonoType::ConstrainedVar(var, constraints) => {
                write!(f, "t{}", var.0)?;
                let names: Vec<_> = constraints
                    .iter()
                    .map(|c| c.trait_fql_name.to_string())
                    .collect();
                if !names.is_empty() {
                    write!(f, " : {}", names.join(" + "))?;
                }
                Ok(())
            }
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
                elements.iter().join(", ").fmt(f)?;
                write!(f, ")")
            }
            MonoType::App { constructor, args } => {
                write!(f, "{}", constructor)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    args.iter().join(", ").fmt(f)?;
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

impl PolyType {
    pub(super) fn generalize_all(ty: MonoType) -> Self {
        let quantified = ty.free_type_vars();

        Self {
            quantified,
            constraints: Vec::new(),
            body: ty,
        }
    }

    /// Instantiate a polytype with fresh type variables
    /// Returns (instantiated_type, fresh_vars_in_order)
    /// The fresh_vars Vec contains the fresh variables in the same order as self.quantified
    fn instantiate(
        &self,
        gen: &mut TypeVarGenerator,
        source_fql: &EPTdFql,
    ) -> (MonoType, Vec<TypeVarId>) {
        if self.quantified.is_empty() {
            return (self.body.clone(), Vec::new());
        }

        let mut subst = unification::Substitution::new();
        let mut fresh_vars = Vec::with_capacity(self.quantified.len());

        for var in &self.quantified {
            let fresh = gen.fresh();
            fresh_vars.push(fresh);
            subst.insert(*var, MonoType::Var(fresh), source_fql);
        }

        (subst.apply(&self.body), fresh_vars)
    }
}

/// Inference context for HM type inference
pub(super) struct HMInferenceContext<'db> {
    pub(super) db: &'db dyn HirInferDatabase,
    /// Type variable generator
    pub(super) type_var_gen: TypeVarGenerator,
    /// The expression currently being inferred by `infer_body_type`.
    /// Used to skip the `infer_value_signature` shortcut for this expression
    /// to prevent Salsa cycles.
    inferring_expr: Option<Fql<hir::Expression>>,
    /// Type equations to be solved
    pub(super) equations: Vec<TypeEquation>,
    /// Type environment (maps expressions/patterns to their types)
    pub(super) type_env: FxHashMap<EPTdFql, MonoType>,
    /// Polymorphic type schemes for let-bound variables
    pub(super) poly_env: FxHashMap<EPTdFql, PolyType>,
    /// Resolution errors collected during inference (FQL + reference path + module_id)
    pub(super) resolution_errors: Vec<HirResolutionError>,
    /// Type variables created for unknown references (resolution errors).
    /// These should be treated as `Missing` rather than `Generic`.
    pub(super) resolution_error_vars: FxHashSet<TypeVarId>,
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
    /// Get the user-visible name for a type variable (for error messages)
    fn get_type_var_name(&self, var_id: TypeVarId) -> String {
        self.type_var_names
            .get(&var_id)
            .map(|name| name.to_string())
            .unwrap_or_else(|| format!("t{}", var_id.0))
    }
}

impl<'db> HMInferenceContext<'db> {
    fn new(db: &'db dyn HirInferDatabase) -> Self {
        Self {
            db,
            type_var_gen: TypeVarGenerator::new(),
            inferring_expr: None,
            equations: Vec::new(),
            type_env: FxHashMap::default(),
            poly_env: FxHashMap::default(),
            resolution_errors: Vec::new(),
            resolution_error_vars: FxHashSet::default(),
            annotation_type_vars: FxHashMap::default(),
            self_type_vars: FxHashMap::default(),
            type_var_names: FxHashMap::default(),
        }
    }

    fn matches_root(&self, fql: &Fql<hir::Expression>) -> bool {
        self.inferring_expr.as_ref() == Some(fql)
    }

    fn maybe_find_type(&mut self, fql: impl Into<EPTdFql>) -> Option<MonoType> {
        let fql = fql.into();
        if let Some(poly_ty) = self.poly_env.get(&fql).cloned() {
            let (instantiated, _fresh_vars) = poly_ty.instantiate(&mut self.type_var_gen, &fql);
            Some(instantiated)
        } else {
            self.type_env.get(&fql).cloned()
        }
    }

    fn generalize_to_poly(
        &mut self,
        mono_ty: MonoType,
        source_fql: impl Into<EPTdFql> + Clone,
    ) -> MonoType {
        if mono_ty.is_polymorphic() {
            let poly_ty = PolyType::generalize_all(mono_ty);
            self.poly_env.insert(source_fql.clone().into(), poly_ty);
            self.maybe_find_type(source_fql)
                .expect("must find poly type just inserted")
        } else {
            self.assign_type(source_fql, mono_ty)
        }
    }

    fn fresh_type_var(&mut self) -> MonoType {
        MonoType::Var(self.type_var_gen.fresh())
    }

    fn unknown_reference(&mut self, err: HirResolutionError, fql: impl Into<EPFql>) -> MonoType {
        self.resolution_errors.push(err);
        let var_id = self.type_var_gen.fresh();
        self.resolution_error_vars.insert(var_id);
        let ty = MonoType::Var(var_id);
        self.assign_type(fql.into(), ty)
    }

    #[must_use]
    fn assign_type(&mut self, fql: impl Into<EPTdFql>, ty: MonoType) -> MonoType {
        self.type_env.insert(fql.into(), ty.clone());
        ty
    }

    /// Add a type equation
    fn add_equation(&mut self, left: MonoType, right: MonoType, fql: impl Into<EPFql>) {
        self.equations.push(TypeEquation {
            left,
            right,
            source: fql.into(),
        });
    }

    /// Get or create a type variable for an annotation type variable (typevar declaration).
    /// Ensures the same Fql<TypeDefinition> always maps to the same TypeVarId.
    fn get_or_create_annotation_type_var(
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
    fn get_or_create_self_type_var(&mut self, trait_fql: Fql<hir::Trait>) -> TypeVarId {
        if let Some(&var_id) = self.self_type_vars.get(&trait_fql) {
            return var_id;
        }
        let var_id = self.type_var_gen.fresh();
        self.self_type_vars.insert(trait_fql, var_id);
        self.type_var_names.insert(var_id, hir::Name::new("Self"));
        var_id
    }
}

/// Convert an AnnotatedType to a MonoType for use in constraint generation.
/// Uses stable Fql<TypeDefinition> identities for type variables, ensuring
/// the same type variable declaration always maps to the same TypeVarId.
fn annotated_to_mono(annotated: &AnnotatedType, ctx: &mut HMInferenceContext) -> Option<MonoType> {
    match annotated {
        AnnotatedType::Missing => None,
        AnnotatedType::Unconstrained => Some(MonoType::Unconstrained),
        AnnotatedType::Unit => Some(MonoType::Unit),
        AnnotatedType::BuiltIn(builtin) => Some(MonoType::Concrete(*builtin)),
        AnnotatedType::TypeDef {
            fql,
            name,
            type_args,
        } => Some(type_def_to_mono(ctx, fql, name, type_args)),
        AnnotatedType::Lambda { arg, ret } => {
            let arg_mono = annotated_to_mono(arg, ctx)?;
            let ret_mono = annotated_to_mono(ret, ctx)?;
            Some(MonoType::Function(Box::new(arg_mono), Box::new(ret_mono)))
        }
        AnnotatedType::Tuple(elements) => {
            let mono_elements: Option<Vec<_>> =
                elements.iter().map(|e| annotated_to_mono(e, ctx)).collect();
            mono_elements.map(MonoType::Tuple)
        }
        AnnotatedType::Bounded { base, args } => {
            let base_mono = annotated_to_mono(base, ctx)?;
            let args_mono: Option<Vec<_>> =
                args.iter().map(|a| annotated_to_mono(a, ctx)).collect();
            Some(MonoType::App {
                constructor: Box::new(base_mono),
                args: args_mono?,
            })
        }
        AnnotatedType::TypeVar(AnnotatedTypeVar { name, fql, .. }) => {
            let var_id = ctx.get_or_create_annotation_type_var(fql.clone(), name.clone());
            Some(MonoType::Var(var_id))
        }
        AnnotatedType::ConstrainedTypeVar {
            base: AnnotatedTypeVar { name, fql, .. },
            constraints,
        } => {
            let var_id = ctx.get_or_create_annotation_type_var(fql.clone(), name.clone());
            Some(MonoType::ConstrainedVar(
                var_id,
                constraints.iter().cloned().collect(),
            ))
        }
        AnnotatedType::SelfType {
            trait_fql,
            trait_constraints,
            ..
        } => {
            let var_id = ctx.get_or_create_self_type_var(trait_fql.clone());
            // let primary = TraitConstraint {
            //     trait_fql: trait_fql.clone(),
            //     trait_fql_name: trait_fql.trait_fql_name(ctx.db),
            //     type_var_constraint_fql: todo!(),
            // };
            // let mut constraints = vec![primary];
            let mut constraints = vec![];
            for constraint in trait_constraints {
                if !constraints.contains(constraint) {
                    constraints.push(constraint.clone());
                }
            }
            Some(MonoType::ConstrainedVar(var_id, constraints))
        }
    }
}

fn type_def_to_mono(
    ctx: &mut HMInferenceContext,
    fql: &Fql<hir::TypeDefinition>,
    name: &hir::Name,
    type_args: &[TypeVarReference],
) -> MonoType {
    let type_args = type_args
        .iter()
        .map(|ty_arg| {
            ctx.get_or_create_annotation_type_var(ty_arg.fql.clone(), ty_arg.name.clone())
        })
        .collect::<Vec<_>>();

    MonoType::TypeDef {
        fql: fql.clone(),
        type_args,
        type_def_name: name.clone(),
    }
}
