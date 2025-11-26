mod expr;
mod pattern;
mod r#trait;
mod type_definition;
mod type_reference;

use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir::Name;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use non_empty_vec::NonEmpty;
use rustc_hash::{FxHashMap, FxHashSet};
use std::hash::Hash;
use text_size::TextRange;

/// Fully Qualified Location - represents an index within a specific module
#[derive(Debug, Clone, Copy)]
pub struct Fql<T> {
    pub module_id: ModuleId,
    pub local_id: Idx<T>,
}

impl<T> Fql<T> {
    pub fn new(module_id: ModuleId, local_id: impl Into<Idx<T>>) -> Self {
        Self {
            module_id,
            local_id: local_id.into(),
        }
    }
}

impl<T> PartialEq for Fql<T> {
    fn eq(&self, other: &Self) -> bool {
        self.module_id == other.module_id && self.local_id == other.local_id
    }
}

impl<T> Eq for Fql<T> {}

impl<T> Hash for Fql<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module_id.hash(state);
        self.local_id.hash(state);
    }
}

// ============================================================================
// Hindley-Milner Type System
// ============================================================================

/// A type variable ID for unification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(usize);

impl TypeVarId {
    fn new(id: usize) -> Self {
        Self(id)
    }
}

/// A monotype - types without quantifiers
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    /// Type variable (e.g., `'a`, `'b`)
    Var(TypeVarId),
    /// Concrete type (e.g., `Int`, `String`)
    Concrete(hir::BuiltInType),
    /// Function type (e.g., `Int -> String`)
    Function(Box<MonoType>, Box<MonoType>),
    /// Tuple type (e.g., `(Int, String)`)
    Tuple(Vec<MonoType>),
    /// Type constructor application (e.g., `Option[Int]`, `Either[String, Int]`)
    App {
        constructor: Fql<hir::TypeReference>,
        args: Vec<MonoType>,
    },
    /// Unit type
    Unit,
}

/// A polytype (type scheme) with quantified variables
/// e.g., `forall a b. a -> b -> (a, b)`
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
    left: MonoType,
    right: MonoType,
    /// Source location for error reporting
    source: ExpressionOrPatternIdx,
}

/// Substitution mapping type variables to types
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    map: FxHashMap<TypeVarId, MonoType>,
}

impl Substitution {
    fn new() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }

    fn insert(&mut self, var: TypeVarId, ty: MonoType) {
        self.map.insert(var, ty);
    }

    fn get(&self, var: TypeVarId) -> Option<&MonoType> {
        self.map.get(&var)
    }

    fn apply(&self, ty: &MonoType) -> MonoType {
        match ty {
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
                constructor: constructor.clone(),
                args: args.iter().map(|t| self.apply(t)).collect(),
            },
            MonoType::Concrete(_) | MonoType::Unit => ty.clone(),
        }
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

/// Unification algorithm with occurs check
fn unify_types(t1: &MonoType, t2: &MonoType) -> Result<Substitution, UnificationError> {
    match (t1, t2) {
        // Same type variable
        (MonoType::Var(v1), MonoType::Var(v2)) if v1 == v2 => Ok(Substitution::new()),

        // Bind type variable to type
        (MonoType::Var(v), t) | (t, MonoType::Var(v)) => {
            if occurs(*v, t) {
                Err(UnificationError::OccursCheck(*v, t.clone()))
            } else {
                let mut subst = Substitution::new();
                subst.insert(*v, t.clone());
                Ok(subst)
            }
        }

        // Function types
        (MonoType::Function(arg1, ret1), MonoType::Function(arg2, ret2)) => {
            let subst1 = unify_types(arg1, arg2)?;
            let ret1_subst = subst1.apply(ret1);
            let ret2_subst = subst1.apply(ret2);
            let subst2 = unify_types(&ret1_subst, &ret2_subst)?;
            Ok(subst1.compose(&subst2))
        }

        // Tuple types
        (MonoType::Tuple(ts1), MonoType::Tuple(ts2)) if ts1.len() == ts2.len() => {
            let mut subst = Substitution::new();
            for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                let t1_subst = subst.apply(t1);
                let t2_subst = subst.apply(t2);
                let new_subst = unify_types(&t1_subst, &t2_subst)?;
                subst = subst.compose(&new_subst);
            }
            Ok(subst)
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
        ) if c1 == c2 && args1.len() == args2.len() => {
            let mut subst = Substitution::new();
            for (t1, t2) in args1.iter().zip(args2.iter()) {
                let t1_subst = subst.apply(t1);
                let t2_subst = subst.apply(t2);
                let new_subst = unify_types(&t1_subst, &t2_subst)?;
                subst = subst.compose(&new_subst);
            }
            Ok(subst)
        }

        // Concrete types
        (MonoType::Concrete(c1), MonoType::Concrete(c2)) if c1 == c2 => Ok(Substitution::new()),

        // Unit types
        (MonoType::Unit, MonoType::Unit) => Ok(Substitution::new()),

        // Mismatch
        _ => Err(UnificationError::TypeMismatch(t1.clone(), t2.clone())),
    }
}

/// Check if a type variable occurs in a type (prevents infinite types)
fn occurs(var: TypeVarId, ty: &MonoType) -> bool {
    match ty {
        MonoType::Var(v) => *v == var,
        MonoType::Function(arg, ret) => occurs(var, arg) || occurs(var, ret),
        MonoType::Tuple(tys) => tys.iter().any(|t| occurs(var, t)),
        MonoType::App { args, .. } => args.iter().any(|t| occurs(var, t)),
        MonoType::Concrete(_) | MonoType::Unit => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnificationError {
    /// Occurs check failed (would create infinite type)
    OccursCheck(TypeVarId, MonoType),
    /// Types don't match
    TypeMismatch(MonoType, MonoType),
}

/// Solve a list of type equations
fn solve_equations(equations: Vec<TypeEquation>) -> Result<Substitution, UnificationError> {
    let mut subst = Substitution::new();

    for equation in equations {
        let left = subst.apply(&equation.left);
        let right = subst.apply(&equation.right);
        let new_subst = unify_types(&left, &right)?;
        subst = subst.compose(&new_subst);
    }

    Ok(subst)
}

/// Get all free type variables in a type
fn free_type_vars(ty: &MonoType) -> FxHashSet<TypeVarId> {
    match ty {
        MonoType::Var(v) => {
            let mut set = FxHashSet::default();
            set.insert(*v);
            set
        }
        MonoType::Function(arg, ret) => {
            let mut vars = free_type_vars(arg);
            vars.extend(free_type_vars(ret));
            vars
        }
        MonoType::Tuple(tys) => {
            let mut vars = FxHashSet::default();
            for t in tys {
                vars.extend(free_type_vars(t));
            }
            vars
        }
        MonoType::App { args, .. } => {
            let mut vars = FxHashSet::default();
            for t in args {
                vars.extend(free_type_vars(t));
            }
            vars
        }
        MonoType::Concrete(_) | MonoType::Unit => FxHashSet::default(),
    }
}

impl PolyType {
    /// Create a monomorphic polytype (no quantified variables)
    fn mono(ty: MonoType) -> Self {
        Self {
            quantified: Vec::new(),
            constraints: Vec::new(),
            body: ty,
        }
    }

    /// Generalize a monotype into a polytype by quantifying free variables
    /// that aren't in the environment
    fn generalize(ty: MonoType, env_vars: &FxHashSet<TypeVarId>) -> Self {
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

    /// Instantiate a polytype by replacing quantified variables with fresh type variables
    fn instantiate(&self, gen: &mut TypeVarGenerator) -> MonoType {
        if self.quantified.is_empty() {
            return self.body.clone();
        }

        // Create fresh type variables for each quantified variable
        let mut subst = Substitution::new();
        for var in &self.quantified {
            subst.insert(*var, MonoType::Var(gen.fresh()));
        }

        subst.apply(&self.body)
    }
}

/// New HM-style inference context
pub struct HMInferenceContext<'db> {
    db: &'db dyn HirTyDatabase,
    /// Type variable generator
    type_var_gen: TypeVarGenerator,
    /// Type equations to be solved
    equations: Vec<TypeEquation>,
    /// Type environment (maps expressions/patterns to their types)
    type_env: FxHashMap<ExpressionOrPatternIdx, MonoType>,
    /// Polymorphic type schemes for let-bound variables
    poly_env: FxHashMap<ExpressionOrPatternIdx, PolyType>,
}

impl<'db> HMInferenceContext<'db> {
    fn new(db: &'db dyn HirTyDatabase) -> Self {
        Self {
            db,
            type_var_gen: TypeVarGenerator::new(),
            equations: Vec::new(),
            type_env: FxHashMap::default(),
            poly_env: FxHashMap::default(),
        }
    }

    /// Generate a fresh type variable
    fn fresh_type_var(&mut self) -> MonoType {
        MonoType::Var(self.type_var_gen.fresh())
    }

    /// Assign a type to an expression or pattern
    fn assign_type(&mut self, id: ExpressionOrPatternIdx, ty: MonoType) {
        self.type_env.insert(id, ty);
    }

    /// Get the type of an expression or pattern
    fn get_type(&self, id: &ExpressionOrPatternIdx) -> Option<&MonoType> {
        self.type_env.get(id)
    }

    /// Add a type equation
    fn add_equation(&mut self, left: MonoType, right: MonoType, source: ExpressionOrPatternIdx) {
        self.equations.push(TypeEquation { left, right, source });
    }

    /// Generalize a type for let-binding
    fn generalize_type(&self, ty: MonoType) -> PolyType {
        // Get all free variables in the current environment
        let mut env_vars = FxHashSet::default();
        for t in self.type_env.values() {
            env_vars.extend(free_type_vars(t));
        }
        PolyType::generalize(ty, &env_vars)
    }

    /// Instantiate a polymorphic type
    fn instantiate_poly(&mut self, poly: &PolyType) -> MonoType {
        poly.instantiate(&mut self.type_var_gen)
    }
}

/// Generate constraints for an expression using HM inference
fn infer_expr_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
    expr: &hir::Expression,
) -> MonoType {
    let fql = Fql::new(module_id, expr_id);
    let idx = ExpressionOrPatternIdx::Expression(fql);

    match expr {
        hir::Expression::Literal(lit) => {
            let ty = match lit {
                hir::Literal::Int(_) => MonoType::Concrete(hir::BuiltInType::Int),
                hir::Literal::Fraction(_) => MonoType::Concrete(hir::BuiltInType::Fraction),
                hir::Literal::String(_) => MonoType::Concrete(hir::BuiltInType::String),
                hir::Literal::Char(_) => MonoType::Concrete(hir::BuiltInType::Char),
            };
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Expression::Unit => {
            let ty = MonoType::Unit;
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Expression::VariableRef { path, scope } => {
            // Look up the variable in the polymorphic environment
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Extract the last name from the path
            let name = match path {
                hir::Path::ThisModule(names) | hir::Path::Unknown(names) => names.last(),
                hir::Path::OtherModule(_) => {
                    // For now, use a fresh type variable for cross-module references
                    let ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, ty.clone());
                    return ty;
                }
            };

            // Try to find as an expression first
            if let Some((var_id, _)) = hir_module.get_expression_by_name(name, *scope) {
                let var_fql = Fql::new(module_id, var_id);
                let var_idx = ExpressionOrPatternIdx::Expression(var_fql);

                // Check if we have a polymorphic type for this variable
                if let Some(poly_ty) = ctx.poly_env.get(&var_idx) {
                    // Instantiate with fresh type variables
                    let ty = poly_ty.instantiate(&mut ctx.type_var_gen);
                    ctx.assign_type(idx, ty.clone());
                    ty
                } else if let Some(mono_ty) = ctx.type_env.get(&var_idx).cloned() {
                    // Create a fresh type variable for this reference and add an equation
                    // This allows bidirectional information flow
                    let ref_ty = ctx.fresh_type_var();
                    ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
                    ctx.assign_type(idx, ref_ty.clone());
                    ref_ty
                } else {
                    // Variable not found in environment, create fresh type variable
                    let ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, ty.clone());
                    ty
                }
            } else if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *scope) {
                // Try to find as a pattern (e.g., lambda parameter)
                let pat_fql = Fql::new(module_id, pat_id);
                let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

                if let Some(mono_ty) = ctx.type_env.get(&pat_idx).cloned() {
                    // Create a fresh type variable for this reference and add an equation
                    let ref_ty = ctx.fresh_type_var();
                    ctx.add_equation(ref_ty.clone(), mono_ty, idx.clone());
                    ctx.assign_type(idx, ref_ty.clone());
                    ref_ty
                } else {
                    // Pattern not found in environment, create fresh type variable
                    let ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, ty.clone());
                    ty
                }
            } else {
                // Variable not found in scope, create fresh type variable
                let ty = ctx.fresh_type_var();
                ctx.assign_type(idx, ty.clone());
                ty
            }
        }

        hir::Expression::Lambda { args, body } => {
            // Each lambda parameter gets a fresh type variable
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            let mut arg_types = Vec::new();
            for arg_id in args {
                let arg_pattern = hir_module.get_pattern(*arg_id);
                let arg_ty = infer_pattern_hm(ctx, module_id, *arg_id, arg_pattern);
                arg_types.push(arg_ty);
            }

            // Infer the body type
            let body_expr = hir_module.get_expression(*body);
            let body_ty = infer_expr_hm(ctx, module_id, *body, body_expr);

            // Build curried function type: arg1 -> (arg2 -> (... -> body))
            let mut func_ty = body_ty;
            for arg_ty in arg_types.into_iter().rev() {
                func_ty = MonoType::Function(Box::new(arg_ty), Box::new(func_ty));
            }

            ctx.assign_type(idx, func_ty.clone());
            func_ty
        }

        hir::Expression::FunctionCall { target, scope, args } => {
            // Infer the target function
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Extract the last name from the path
            let name = match target {
                hir::Path::ThisModule(names) | hir::Path::Unknown(names) => names.last(),
                hir::Path::OtherModule(_) => {
                    // For cross-module references, create a fresh function type
                    let (hir_module, _) = hir::lower_file(ctx.db, module_id);

                    let mut arg_types = Vec::new();
                    for arg_id in args {
                        let arg_expr = hir_module.get_expression(*arg_id);
                        let arg_ty = infer_expr_hm(ctx, module_id, *arg_id, arg_expr);
                        arg_types.push(arg_ty);
                    }

                    let result_ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, result_ty.clone());
                    return result_ty;
                }
            };

            let func_ty = if let Some((func_id, _)) = hir_module.get_expression_by_name(name, *scope) {
                let func_expr = hir_module.get_expression(func_id);
                infer_expr_hm(ctx, module_id, func_id, func_expr)
            } else {
                ctx.fresh_type_var()
            };

            // Infer argument types
            let mut arg_types = Vec::new();
            for arg_id in args {
                let arg_expr = hir_module.get_expression(*arg_id);
                let arg_ty = infer_expr_hm(ctx, module_id, *arg_id, arg_expr);
                arg_types.push(arg_ty);
            }

            // Build expected function type: arg1 -> (arg2 -> (... -> result))
            let result_ty = ctx.fresh_type_var();
            let mut expected_func_ty = result_ty.clone();
            for arg_ty in arg_types.into_iter().rev() {
                expected_func_ty = MonoType::Function(Box::new(arg_ty), Box::new(expected_func_ty));
            }

            // Add equation: func_ty = arg1 -> ... -> result
            ctx.add_equation(func_ty, expected_func_ty, idx.clone());

            ctx.assign_type(idx, result_ty.clone());
            result_ty
        }

        hir::Expression::Binary { lhs, rhs, .. } => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            let lhs_expr = hir_module.get_expression(*lhs);
            let lhs_ty = infer_expr_hm(ctx, module_id, *lhs, lhs_expr);

            let rhs_expr = hir_module.get_expression(*rhs);
            let rhs_ty = infer_expr_hm(ctx, module_id, *rhs, rhs_expr);

            // For now, assume both sides have the same type and return that type
            ctx.add_equation(lhs_ty.clone(), rhs_ty.clone(), idx.clone());

            let result_ty = lhs_ty;
            ctx.assign_type(idx, result_ty.clone());
            result_ty
        }

        hir::Expression::Tuple(elements) => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            let mut element_types = Vec::new();
            for elem_id in elements {
                let elem_expr = hir_module.get_expression(*elem_id);
                let elem_ty = infer_expr_hm(ctx, module_id, *elem_id, elem_expr);
                element_types.push(elem_ty);
            }

            let ty = MonoType::Tuple(element_types);
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Expression::IfThenElse { condition, then, else_ } => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Infer condition type and constrain it to Bool
            let cond_expr = hir_module.get_expression(*condition);
            let cond_ty = infer_expr_hm(ctx, module_id, *condition, cond_expr);
            ctx.add_equation(cond_ty, MonoType::Concrete(hir::BuiltInType::Bool), idx.clone());

            // Infer then branch
            let then_expr = hir_module.get_expression(*then);
            let then_ty = infer_expr_hm(ctx, module_id, *then, then_expr);

            // Infer else branch
            let else_expr = hir_module.get_expression(*else_);
            let else_ty = infer_expr_hm(ctx, module_id, *else_, else_expr);

            // Both branches must have the same type
            ctx.add_equation(then_ty.clone(), else_ty, idx.clone());

            ctx.assign_type(idx, then_ty.clone());
            then_ty
        }

        hir::Expression::Unary { expression, .. } => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // For unary operations, infer the inner expression type
            let inner = hir_module.get_expression(*expression);
            let inner_ty = infer_expr_hm(ctx, module_id, *expression, inner);

            // The result has the same type as the inner expression
            ctx.assign_type(idx, inner_ty.clone());
            inner_ty
        }

        hir::Expression::Match { condition, targets } => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Infer the scrutinee type
            let value_expr = hir_module.get_expression(*condition);
            let value_ty = infer_expr_hm(ctx, module_id, *condition, value_expr);

            // Infer all arm patterns and bodies
            let result_ty = ctx.fresh_type_var();

            for (pattern_id, body_id) in targets {
                // Pattern must match the scrutinee type
                let pattern = hir_module.get_pattern(*pattern_id);
                let pattern_ty = infer_pattern_hm(ctx, module_id, *pattern_id, pattern);
                ctx.add_equation(pattern_ty, value_ty.clone(), idx.clone());

                // Body must have the same type as other arms
                let body_expr = hir_module.get_expression(*body_id);
                let body_ty = infer_expr_hm(ctx, module_id, *body_id, body_expr);
                ctx.add_equation(body_ty, result_ty.clone(), idx.clone());
            }

            ctx.assign_type(idx, result_ty.clone());
            result_ty
        }

        hir::Expression::Missing => {
            // Missing expressions get a fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }
    }
}

/// Generate constraints for a pattern using HM inference
fn infer_pattern_hm(
    ctx: &mut HMInferenceContext,
    module_id: ModuleId,
    pattern_id: hir::PatternIdx,
    pattern: &hir::Pattern,
) -> MonoType {
    let fql = Fql::new(module_id, pattern_id);
    let idx = ExpressionOrPatternIdx::Pattern(fql);

    match pattern {
        hir::Pattern::Literal(lit) => {
            let ty = match lit {
                hir::Literal::Int(_) => MonoType::Concrete(hir::BuiltInType::Int),
                hir::Literal::Fraction(_) => MonoType::Concrete(hir::BuiltInType::Fraction),
                hir::Literal::String(_) => MonoType::Concrete(hir::BuiltInType::String),
                hir::Literal::Char(_) => MonoType::Concrete(hir::BuiltInType::Char),
            };
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::Unit => {
            let ty = MonoType::Unit;
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::VariableDeclaration { .. } => {
            // Fresh type variable for the bound variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::Tuple(elements) => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            let mut element_types = Vec::new();
            for elem_id in elements {
                let elem_pattern = hir_module.get_pattern(*elem_id);
                let elem_ty = infer_pattern_hm(ctx, module_id, *elem_id, elem_pattern);
                element_types.push(elem_ty);
            }

            let ty = MonoType::Tuple(element_types);
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::PatternRef { path, scope } => {
            // Look up the pattern in the environment
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Extract the last name from the path
            let name = match path {
                hir::Path::ThisModule(names) | hir::Path::Unknown(names) => names.last(),
                hir::Path::OtherModule(_) => {
                    // For now, use a fresh type variable for cross-module references
                    let ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, ty.clone());
                    return ty;
                }
            };

            if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                let pat_idx = ExpressionOrPatternIdx::Pattern(pat_fql);

                // Check if we have a type for this pattern
                if let Some(mono_ty) = ctx.type_env.get(&pat_idx) {
                    let ty = mono_ty.clone();
                    ctx.assign_type(idx, ty.clone());
                    ty
                } else {
                    // Pattern not found in environment, create fresh type variable
                    let ty = ctx.fresh_type_var();
                    ctx.assign_type(idx, ty.clone());
                    ty
                }
            } else {
                // Pattern not found in scope, create fresh type variable
                let ty = ctx.fresh_type_var();
                ctx.assign_type(idx, ty.clone());
                ty
            }
        }

        hir::Pattern::Destructure { target, scope, args } => {
            let (hir_module, _) = hir::lower_file(ctx.db, module_id);

            // Infer types for all fields
            let mut _field_types = Vec::new();
            for field_id in args {
                let field_pattern = hir_module.get_pattern(*field_id);
                let field_ty = infer_pattern_hm(ctx, module_id, *field_id, field_pattern);
                _field_types.push(field_ty);
            }

            // For now, create a fresh type variable for the constructor application
            // In a full implementation, we'd look up the constructor's type scheme from target and scope
            let _ = (target, scope); // Suppress unused warnings
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::Nil => {
            // Nil pattern represents an empty list
            // In a full implementation, this would be List[a] where a is fresh
            // For now, just use a fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }

        hir::Pattern::Missing => {
            // Missing patterns get a fresh type variable
            let ty = ctx.fresh_type_var();
            ctx.assign_type(idx, ty.clone());
            ty
        }
    }
}

/// Main Hindley-Milner type inference function for a module
/// This will eventually replace the old `infer_types` function
pub fn infer_types_hm(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let mut ctx = HMInferenceContext::new(db);
    let (hir_module, _) = hir::lower_file(db, module_id);

    // Phase 1: Generate constraints for all top-level expressions
    // We iterate through expressions in the order they appear in the module
    for (expression_id, expression, _range, _name_op) in hir_module.expressions() {
        infer_expr_hm(&mut ctx, module_id, expression_id, expression);
    }

    // Phase 2: Solve all accumulated type equations
    match solve_equations(ctx.equations.clone()) {
        Ok(substitution) => {
            // Phase 3: Apply the substitution to all types in the environment
            let mut result = HirTypedModule::empty();

            for (expression_id, _expression, range, name_op) in hir_module.expressions() {
                let fql = Fql::new(module_id, expression_id);
                let idx = ExpressionOrPatternIdx::Expression(fql);

                if let Some(mono_ty) = ctx.type_env.get(&idx) {
                    let resolved_mono = substitution.apply(mono_ty);
                    let resolved_type = mono_to_resolved(&resolved_mono);
                    result.expression_types.insert(expression_id, resolved_type.clone());

                    // Check for type annotation conflicts
                    check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
                }
            }

            for (pattern_id, _pattern, range, name_op) in hir_module.patterns() {
                let fql = Fql::new(module_id, pattern_id);
                let idx = ExpressionOrPatternIdx::Pattern(fql);

                if let Some(mono_ty) = ctx.type_env.get(&idx) {
                    let resolved_mono = substitution.apply(mono_ty);
                    let resolved_type = mono_to_resolved(&resolved_mono);
                    result.pattern_types.insert(pattern_id, resolved_type.clone());

                    // Check for type annotation conflicts
                    check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
                }
            }

            result
        }
        Err(_unification_error) => {
            // If unification fails, return an empty result with errors
            // TODO: Convert unification errors to proper type inference errors
            HirTypedModule::empty()
        }
    }
}

/// Convert a MonoType to a ResolvedType (for compatibility with old system)
fn mono_to_resolved(mono: &MonoType) -> ResolvedType {
    match mono {
        MonoType::Var(_) => ResolvedType::Unknown,
        MonoType::Concrete(builtin) => ResolvedType::BuiltIn(*builtin),
        MonoType::Function(arg, ret) => ResolvedType::Lambda {
            arg_type: Box::new(mono_to_resolved(arg)),
            return_type: Box::new(mono_to_resolved(ret)),
        },
        MonoType::Tuple(elements) => {
            let resolved_elements: Vec<_> = elements.iter().map(mono_to_resolved).collect();
            if resolved_elements.is_empty() {
                ResolvedType::Unknown
            } else {
                let first = resolved_elements[0].clone();
                let rest = resolved_elements.into_iter().skip(1).collect();
                ResolvedType::Tuple(NonEmpty::from((first, rest)))
            }
        }
        MonoType::App { constructor, args: _ } => {
            // For now, convert to Named type
            ResolvedType::Named(constructor.clone())
        }
        MonoType::Unit => ResolvedType::Unit,
    }
}

// ============================================================================
// Old Type System (to be gradually replaced)
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Unit,
    Named(Fql<hir::TypeReference>),
    BuiltIn(hir::BuiltInType),
    Lambda {
        arg_type: Box<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Tuple(NonEmpty<ResolvedType>),
    Bounded {
        base: Fql<hir::TypeReference>,
        args: Vec<Fql<hir::TypeReference>>,
    },
}

struct InferenceContext<'db> {
    db: &'db dyn HirTyDatabase,
    type_requirements: FxHashMap<ExpressionOrPatternIdx, Vec<TypeRequirements>>,
    resolved_types: FxHashMap<ExpressionOrPatternIdx, ResolvedType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeRequirements {
    MustBeType(ResolvedType),
    MustBeSameAs(ExpressionOrPatternIdx),
    MustImplementTrait(Fql<hir::Trait>),
    Tuple(NonEmpty<Fql<hir::Expression>>),
    Variable(usize),
    Lambda {
        args: Vec<Fql<hir::Pattern>>,
        body: Fql<hir::Expression>,
    },
    FunctionCall {
        func: ExpressionOrPatternIdx,
        args: Vec<Fql<hir::Expression>>,
    },
}

/// A fully qualified reference to an expression or pattern within a specific module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExpressionOrPatternIdx {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
}

impl<'db> InferenceContext<'db> {
    fn new(db: &'db dyn HirTyDatabase) -> Self {
        Self {
            db,
            type_requirements: FxHashMap::default(),
            resolved_types: FxHashMap::default(),
        }
    }

    fn insert_expr_type_variable(
        &mut self,
        module_id: ModuleId,
        expression_id: hir::ExpressionIdx,
    ) {
        self.add_expr_requirements(
            module_id,
            expression_id,
            TypeRequirements::Variable(self.type_requirements.len()),
        );
    }

    fn insert_pattern_type_variable(&mut self, module_id: ModuleId, pattern_id: hir::PatternIdx) {
        self.add_pattern_requirements(
            module_id,
            pattern_id,
            TypeRequirements::Variable(self.type_requirements.len()),
        );
    }

    fn insert_type(
        &mut self,
        module_id: ModuleId,
        expression_id: hir::ExpressionIdx,
        resolved_type: ResolvedType,
    ) {
        println!("Inserting {expression_id:?} -> {resolved_type:?}");
        self.add_expr_requirements(
            module_id,
            expression_id,
            TypeRequirements::MustBeType(resolved_type),
        );
    }

    fn add_bidirectional_binding(
        &mut self,
        this: ExpressionOrPatternIdx,
        other: ExpressionOrPatternIdx,
    ) {
        self.add_pattern_expr_requirements(
            this.clone(),
            TypeRequirements::MustBeSameAs(other.clone()),
        );
        self.add_pattern_expr_requirements(other, TypeRequirements::MustBeSameAs(this));
    }

    fn add_expr_requirements(
        &mut self,
        module_id: ModuleId,
        expression_id: hir::ExpressionIdx,
        constraint: TypeRequirements,
    ) {
        let fql = Fql::new(module_id, expression_id);
        let idx = ExpressionOrPatternIdx::Expression(fql);
        self.add_pattern_expr_requirements(idx, constraint);
    }

    fn add_pattern_requirements(
        &mut self,
        module_id: ModuleId,
        pattern_id: hir::PatternIdx,
        constraint: TypeRequirements,
    ) {
        let fql = Fql::new(module_id, pattern_id);
        let idx = ExpressionOrPatternIdx::Pattern(fql);
        self.add_pattern_expr_requirements(idx, constraint);
    }

    fn add_pattern_expr_requirements(
        &mut self,
        idx: ExpressionOrPatternIdx,
        constraint: TypeRequirements,
    ) {
        let constraints = self.type_requirements.entry(idx).or_insert_with(Vec::new);

        // Only add if not already present
        if !constraints.contains(&constraint) {
            constraints.push(constraint);
        }
    }
}

#[must_use]
pub fn infer_types(db: &dyn HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
    let mut result = HirTypedModule::empty();

    let (hir_module, _) = hir::lower_file(db, module_id);

    let mut ctx = InferenceContext::new(db);

    // Collect type requirements for all expressions
    for (expression_id, _expression, _range, _name_op) in hir_module.expressions() {
        expr::collect_expr_type(&mut ctx, module_id, expression_id);
    }

    // Collect type requirements for all patterns
    for (pattern_id, _pattern, _range, _name_op) in hir_module.patterns() {
        pattern::collect_pattern_type(&mut ctx, module_id, pattern_id);
    }

    // Unify and resolve types for all expressions
    for (expression_id, _expression, range, name_op) in hir_module.expressions() {
        let idx = ExpressionOrPatternIdx::Expression(Fql::new(module_id, expression_id));
        if ctx.type_requirements.contains_key(&idx) {
            let resolved_type = unify(&mut ctx, idx);
            result
                .expression_types
                .insert(expression_id, resolved_type.clone());

            check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
        }
    }

    // Unify and resolve types for all patterns
    for (pattern_id, _pattern, range, name_op) in hir_module.patterns() {
        let idx = ExpressionOrPatternIdx::Pattern(Fql::new(module_id, pattern_id));
        if ctx.type_requirements.contains_key(&idx) {
            let resolved_type = unify(&mut ctx, idx);
            result
                .pattern_types
                .insert(pattern_id, resolved_type.clone());

            check_type_annotation(db, &mut result, module_id, range, name_op, resolved_type);
        }
    }

    result
}

fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    name_op: Option<(Name, ScopeIdx)>,
    resolved_type: ResolvedType,
) {
    // Check for type annotation conflicts
    if let Some((name, scope)) = name_op {
        let expected_type = type_reference::type_reference_to_resolved(
            db,
            current_module_id,
            &hir::Path::ThisModule(NonEmpty::new(name.clone())),
            scope,
        );
        if expected_type != ResolvedType::Unknown && expected_type != resolved_type {
            result.error(
                crate::diagnostics::TypeInferenceErrorKind::ConflictingTypeAnnotation {
                    expected: expected_type,
                    found: resolved_type,
                },
                range,
            );
        }
    }
}

fn unify(ctx: &mut InferenceContext, id: ExpressionOrPatternIdx) -> ResolvedType {
    let mut visited = FxHashSet::default();
    unify_with_visited(ctx, id, &mut visited)
}

fn unify_with_visited(
    ctx: &mut InferenceContext,
    id: ExpressionOrPatternIdx,
    visited: &mut FxHashSet<ExpressionOrPatternIdx>,
) -> ResolvedType {
    // Check cache first
    if let Some(ty) = ctx.resolved_types.get(&id) {
        if *ty != ResolvedType::Unknown {
            return ty.clone();
        }
    }

    // Check for cycles
    if visited.contains(&id) {
        println!("Cycle detected for {id:?}, returning Unknown");
        return ResolvedType::Unknown;
    }
    visited.insert(id.clone());

    let constraints = ctx
        .type_requirements
        .get(&id)
        .unwrap_or_else(|| {
            panic!(
                "no constraints for {:?}. all constraints: {:?}",
                id, ctx.type_requirements
            )
        })
        .clone();

    println!("Unifying {id:?} with constraints: {constraints:?}");

    // Compute the resolved type based on constraints
    let resolved_type = compute_type(ctx, &id, &constraints, visited);

    println!("Unified {id:?} as type: {resolved_type:?}");

    // Cache non-Unknown results
    if resolved_type != ResolvedType::Unknown {
        ctx.resolved_types.insert(id, resolved_type.clone());
    }

    resolved_type
}

fn compute_type(
    ctx: &mut InferenceContext,
    id: &ExpressionOrPatternIdx,
    constraints: &[TypeRequirements],
    visited: &mut FxHashSet<ExpressionOrPatternIdx>,
) -> ResolvedType {
    // Priority 1: MustBeType constraints (most specific)
    let must_be_types = constraints
        .iter()
        .filter_map(|req| {
            if let TypeRequirements::MustBeType(ty) = req {
                Some(ty)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if !must_be_types.is_empty() {
        if must_be_types.len() > 1 {
            panic!("more than one 'MustBeType' constraint");
        }
        return must_be_types[0].clone();
    }

    // Priority 2: Tuple constraints
    if let Some(TypeRequirements::Tuple(inners)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Tuple(_)))
    {
        unsafe {
            let inner_types = inners
                .into_iter()
                .map(|inner_id| unify_with_visited(ctx, ExpressionOrPatternIdx::Expression(inner_id.clone()), visited))
                .collect();

            return ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types));
        }
    }

    // Priority 3: Lambda constraints
    if let Some(TypeRequirements::Lambda { args, body }) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Lambda { .. }))
    {
        // Build curried lambda type: arg1 -> (arg2 -> (... -> body_type))
        let body_type = unify_with_visited(ctx, ExpressionOrPatternIdx::Expression(body.clone()), visited);

        // Work backwards through arguments to build nested lambda types
        let mut result_type = body_type;
        for arg in args.iter().rev() {
            let arg_type = unify_with_visited(ctx, ExpressionOrPatternIdx::Pattern(arg.clone()), visited);
            result_type = ResolvedType::Lambda {
                arg_type: Box::new(arg_type),
                return_type: Box::new(result_type),
            };
        }

        return result_type;
    }

    // Priority 3.5: FunctionCall constraints (bidirectional type inference)
    if let Some(TypeRequirements::FunctionCall { func, args }) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::FunctionCall { .. }))
    {
        // Get the function's type (works for both expressions and patterns)
        let mut func_type = unify_with_visited(ctx, func.clone(), visited);

        // Apply each argument to unwrap the curried lambda type
        for arg in args {
            match func_type {
                ResolvedType::Lambda {
                    arg_type: _,
                    return_type,
                } => {
                    // The argument type must match the parameter type (bidirectional constraint)
                    // This happens automatically through unification
                    let _arg_ty = unify_with_visited(ctx, ExpressionOrPatternIdx::Expression(arg.clone()), visited);

                    // Move to the return type for the next argument
                    func_type = *return_type;
                }
                _ => {
                    // If we don't have a lambda type, we can't determine the return type
                    // This might happen if the function type hasn't been fully inferred yet
                    break;
                }
            }
        }

        return func_type;
    }

    // Priority 4: MustBeSameAs constraints (follow references)
    if let Some(TypeRequirements::MustBeSameAs(other_id)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::MustBeSameAs(_)))
    {
        println!("Unifying {id:?} must be same as {other_id:?}");
        // The cycle check happens at the top of unify_with_visited
        return unify_with_visited(ctx, other_id.clone(), visited);
    }

    // Priority 5: Trait constraints
    if let Some(TypeRequirements::MustImplementTrait(_)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::MustImplementTrait(_)))
    {
        todo!("trait impl");
    }

    // Priority 7: Variables (least specific - only if nothing else constrains it)
    if let Some(TypeRequirements::Variable(_)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Variable(_)))
    {
        return ResolvedType::Unknown;
    }

    ResolvedType::Unknown
}
