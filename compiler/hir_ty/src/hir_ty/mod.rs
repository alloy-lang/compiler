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
use rustc_hash::FxHashMap;
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeRequirements {
    MustBeType(ResolvedType),
    MustBeSameAs(ExpressionOrPatternIdx),
    MustImplementTrait(Fql<hir::Trait>),
    Annotated(Fql<hir::TypeReference>),
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
        if ctx
            .type_requirements
            .contains_key(&ExpressionOrPatternIdx::Expression(Fql::new(
                module_id,
                expression_id,
            )))
        {
            let resolved_type = unify(
                &ctx,
                ExpressionOrPatternIdx::Expression(Fql::new(module_id, expression_id)),
            );
            result
                .expression_types
                .insert(expression_id, resolved_type.clone());

            check_type_annotation(&mut result, &hir_module, range, name_op, resolved_type);
        }
    }

    // Unify and resolve types for all patterns
    for (pattern_id, _pattern, range, name_op) in hir_module.patterns() {
        if ctx
            .type_requirements
            .contains_key(&ExpressionOrPatternIdx::Pattern(Fql::new(
                module_id, pattern_id,
            )))
        {
            let resolved_type = unify(
                &ctx,
                ExpressionOrPatternIdx::Pattern(Fql::new(module_id, pattern_id)),
            );
            result
                .pattern_types
                .insert(pattern_id, resolved_type.clone());

            check_type_annotation(&mut result, &hir_module, range, name_op, resolved_type);
        }
    }

    result
}

fn check_type_annotation(
    result: &mut HirTypedModule,
    hir_module: &hir::HirModule,
    range: TextRange,
    name_op: Option<(Name, ScopeIdx)>,
    resolved_type: ResolvedType,
) {
    // Check for type annotation conflicts
    if let Some((name, scope)) = name_op {
        if let Some((_type_idx, type_ref)) = hir_module.get_type_reference_by_name(&name, scope) {
            let expected_type = type_reference_to_resolved(&hir_module, type_ref);
            if expected_type != resolved_type {
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
}

/// Convert a HIR TypeReference to a ResolvedType
fn type_reference_to_resolved(
    hir_module: &hir::HirModule,
    type_ref: &hir::TypeReference,
) -> ResolvedType {
    match type_ref {
        hir::TypeReference::Unconstrained => ResolvedType::Unknown,
        hir::TypeReference::Missing => ResolvedType::Unknown,
        hir::TypeReference::SelfRef => ResolvedType::Unknown, // TODO: Handle self type
        hir::TypeReference::Unit => ResolvedType::Unit,
        hir::TypeReference::Named(_) => {
            todo!("Resolve named types properly")
        }
        hir::TypeReference::BuiltIn(built_in) => ResolvedType::BuiltIn(*built_in),
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg =
                type_reference_to_resolved(hir_module, hir_module.get_type_reference(*arg_type));
            let ret =
                type_reference_to_resolved(hir_module, hir_module.get_type_reference(*return_type));
            ResolvedType::Lambda {
                arg_type: Box::new(arg),
                return_type: Box::new(ret),
            }
        }
        hir::TypeReference::Tuple(types) => {
            if types.is_empty() {
                ResolvedType::Unit
            } else {
                unsafe {
                    let inner_types: Vec<_> = types
                        .iter()
                        .map(|t| {
                            type_reference_to_resolved(
                                hir_module,
                                hir_module.get_type_reference(*t),
                            )
                        })
                        .collect();
                    ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types))
                }
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            type_reference_to_resolved(hir_module, hir_module.get_type_reference(*inner))
        }
        hir::TypeReference::Bounded { base: _, args: _ } => {
            todo!("Handle bounded types properly")
        }
    }
}

// fn generate_type_equations(
//     ctx: &InferenceContext,
//     hir_module: &hir::HirModule,
//     expression: &hir::Expression,
// ) -> Vec<(hir::ExpressionIdx, ResolvedType, ResolvedType)> {
//     let mut equations = Vec::new();
//
//     for (expression_id, expression, _range, _name_op) in hir_module.expressions() {
//         match expression {
//             hir::Expression::Missing => todo!("Missing expression"),
//             hir::Expression::Literal(_) => {}
//             hir::Expression::VariableRef { .. } => {}
//             hir::Expression::Binary { .. } => {}
//             hir::Expression::Unit => {}
//             hir::Expression::IfThenElse { .. } => {}
//             hir::Expression::Tuple(_) => {}
//             hir::Expression::Unary { .. } => {}
//             hir::Expression::Lambda { .. } => {}
//             hir::Expression::FunctionCall { .. } => {}
//             hir::Expression::Match { .. } => {}
//         }
//     }
//
//     equations
// }

fn unify(ctx: &InferenceContext, id: ExpressionOrPatternIdx) -> ResolvedType {
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
                .map(|inner_id| unify(ctx, ExpressionOrPatternIdx::Expression(inner_id.clone())))
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
        let body_type = unify(ctx, ExpressionOrPatternIdx::Expression(body.clone()));

        // Work backwards through arguments to build nested lambda types
        let mut result_type = body_type;
        for arg in args.iter().rev() {
            let arg_type = unify(ctx, ExpressionOrPatternIdx::Pattern(arg.clone()));
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
        let mut func_type = unify(ctx, func.clone());

        // Apply each argument to unwrap the curried lambda type
        for arg in args {
            match func_type {
                ResolvedType::Lambda {
                    arg_type: _,
                    return_type,
                } => {
                    // The argument type must match the parameter type (bidirectional constraint)
                    // This happens automatically through unification
                    let _arg_ty = unify(ctx, ExpressionOrPatternIdx::Expression(arg.clone()));

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
        return unify(ctx, other_id.clone());
    }

    // Priority 5: Type annotations
    if let Some(TypeRequirements::Annotated(_)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Annotated(_)))
    {
        todo!("Handle type annotations");
    }

    // Priority 6: Trait constraints
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
