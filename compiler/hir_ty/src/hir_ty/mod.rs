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

mod hm;
pub use hm::infer_types_hm;

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
        base: Box<ResolvedType>,
        args: Vec<ResolvedType>,
    },
    /// Generic type variable (for polymorphic types)
    /// The usize represents a canonical type variable ID
    Generic(usize),
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
                .map(|inner_id| {
                    unify_with_visited(
                        ctx,
                        ExpressionOrPatternIdx::Expression(inner_id.clone()),
                        visited,
                    )
                })
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
        let body_type = unify_with_visited(
            ctx,
            ExpressionOrPatternIdx::Expression(body.clone()),
            visited,
        );

        // Work backwards through arguments to build nested lambda types
        let mut result_type = body_type;
        for arg in args.iter().rev() {
            let arg_type =
                unify_with_visited(ctx, ExpressionOrPatternIdx::Pattern(arg.clone()), visited);
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
                    let _arg_ty = unify_with_visited(
                        ctx,
                        ExpressionOrPatternIdx::Expression(arg.clone()),
                        visited,
                    );

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
