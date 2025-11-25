use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir as hir;
use alloy_hir::Name;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedType {
    Unknown,
    Unit,
    Named {
        path: hir::Path,
        id: hir::TypeIdx,
    },
    BuiltIn(hir::BuiltInType),
    Lambda {
        arg_type: Box<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Tuple(NonEmpty<ResolvedType>),
    Bounded {
        base: hir::TypeIdx,
        args: Vec<hir::TypeIdx>,
    },
}

#[derive(Debug)]
pub struct InferenceContext {
    type_requirements: FxHashMap<ExpressionOrPatternIdx, Vec<TypeRequirements>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum TypeRequirements {
    MustBeType(ResolvedType),
    Tuple(NonEmpty<hir::ExpressionIdx>),
    MustBeSameAs(ExpressionOrPatternIdx),
    MustImplementTrait(hir::Path),
    Annotated(hir::Path, hir::TypeIdx),
    Variable(usize),
    Lambda {
        args: Vec<hir::PatternIdx>,
        body: hir::ExpressionIdx,
    },
    FunctionCall {
        func: ExpressionOrPatternIdx,
        args: Vec<hir::ExpressionIdx>,
    },
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum ExpressionOrPatternIdx {
    Expression(hir::ExpressionIdx),
    Pattern(hir::PatternIdx),
}

impl InferenceContext {
    fn new() -> Self {
        Self {
            type_requirements: FxHashMap::default(),
        }
    }

    fn insert_expr_type_variable(&mut self, expression_id: hir::ExpressionIdx) {
        self.add_expr_requirements(
            expression_id,
            TypeRequirements::Variable(self.type_requirements.len()),
        );
    }

    fn insert_pattern_type_variable(&mut self, pattern_id: hir::PatternIdx) {
        self.add_pattern_requirements(
            pattern_id,
            TypeRequirements::Variable(self.type_requirements.len()),
        );
    }

    fn insert_expression_literal_type(
        &mut self,
        expression_id: hir::ExpressionIdx,
        lit: &hir::Literal,
    ) {
        self.add_expr_requirements(
            expression_id,
            TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::from(lit))),
        )
    }

    fn insert_pattern_literal_type(&mut self, pattern_id: hir::PatternIdx, lit: &hir::Literal) {
        self.add_pattern_requirements(
            pattern_id,
            TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::from(lit))),
        )
    }

    fn insert_expr_type_named(
        &mut self,
        expression_id: hir::ExpressionIdx,
        path: hir::Path,
        id: hir::TypeIdx,
    ) {
        self.add_expr_requirements(expression_id, TypeRequirements::Annotated(path, id));
    }

    fn insert_type(&mut self, expression_id: hir::ExpressionIdx, resolved_type: ResolvedType) {
        println!("Inserting {expression_id:?} -> {resolved_type:?}");
        self.add_expr_requirements(expression_id, TypeRequirements::MustBeType(resolved_type));
    }

    fn add_expr_requirements(
        &mut self,
        expression_id: hir::ExpressionIdx,
        constraint: TypeRequirements,
    ) {
        let constraints = self
            .type_requirements
            .entry(ExpressionOrPatternIdx::Expression(expression_id))
            .or_insert_with(Vec::new);

        // Only add if not already present
        if !constraints.contains(&constraint) {
            constraints.push(constraint);
        }
    }

    fn add_pattern_requirements(
        &mut self,
        pattern_id: hir::PatternIdx,
        constraint: TypeRequirements,
    ) {
        let constraints = self
            .type_requirements
            .entry(ExpressionOrPatternIdx::Pattern(pattern_id))
            .or_insert_with(Vec::new);

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

    let mut ctx = InferenceContext::new();

    // Collect type requirements for all expressions
    for (expression_id, expression, _range, _name_op) in hir_module.expressions() {
        collect_expr_type(&mut ctx, &hir_module, expression_id, expression);
    }

    // Collect type requirements for all patterns
    for (pattern_id, pattern, _range, _name_op) in hir_module.patterns() {
        collect_pattern_type(&mut ctx, &hir_module, pattern_id, pattern);
    }

    // Unify and resolve types for all expressions
    for (expression_id, _expression, range, name_op) in hir_module.expressions() {
        if ctx
            .type_requirements
            .contains_key(&ExpressionOrPatternIdx::Expression(expression_id))
        {
            let resolved_type = unify(&ctx, ExpressionOrPatternIdx::Expression(expression_id));
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
            .contains_key(&ExpressionOrPatternIdx::Pattern(pattern_id))
        {
            let resolved_type = unify(&ctx, ExpressionOrPatternIdx::Pattern(pattern_id));
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
        hir::TypeReference::Named(path) => {
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
        hir::TypeReference::Bounded { base, args } => {
            todo!("Handle bounded types properly")
        }
    }
}

fn collect_expr_type(
    ctx: &mut InferenceContext,
    hir_module: &hir::HirModule,
    expression_id: hir::ExpressionIdx,
    expression: &hir::Expression,
) {
    println!("Collecting expression type: {expression:?}. id: {expression_id:?}");
    match expression {
        hir::Expression::Missing => todo!("Missing expression"),
        hir::Expression::Literal(lit) => {
            ctx.insert_expression_literal_type(expression_id, lit);
        }
        hir::Expression::VariableRef { path, scope } => {
            ctx.insert_expr_type_variable(expression_id);
            match path {
                hir::Path::ThisModule(this_path) => {
                    match hir_module.get_type_reference_by_name(this_path.first(), *scope) {
                        None => {
                            println!("No type annotation for expression: {expression:?}. id: {expression_id:?}. path: {path:?}");
                        }
                        Some((type_id, _)) => {
                            ctx.add_expr_requirements(
                                expression_id,
                                TypeRequirements::Annotated(path.clone(), type_id),
                            );
                        }
                    };
                    match hir_module.get_pattern_by_name(this_path.first(), *scope) {
                        None => {
                            println!("No pattern for path: {expression:?}. id: {expression_id:?}. path: {path:?}");
                        }
                        Some((ref_id, _)) => {
                            collect_pattern_type(
                                ctx,
                                hir_module,
                                ref_id,
                                hir_module.get_pattern(ref_id),
                            );
                            ctx.add_expr_requirements(
                                expression_id,
                                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Pattern(
                                    ref_id,
                                )),
                            );
                        }
                    };
                    match hir_module.get_expression_by_name(this_path.first(), *scope) {
                        None => {
                            println!("No expression for path: {expression:?}. id: {expression_id:?}. path: {path:?}");
                        }
                        Some((ref_id, _)) => {
                            collect_expr_type(
                                ctx,
                                hir_module,
                                ref_id,
                                hir_module.get_expression(ref_id),
                            );
                            ctx.add_expr_requirements(
                                expression_id,
                                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(
                                    ref_id,
                                )),
                            );
                        }
                    };
                }
                hir::Path::OtherModule(_) => todo!("unknown path"),
                hir::Path::Unknown(_) => todo!("unknown path"),
            }
        }
        hir::Expression::Binary { op, lhs, rhs } => {
            ctx.insert_expr_type_variable(expression_id);

            collect_expr_type(ctx, hir_module, *lhs, hir_module.get_expression(*lhs));
            collect_expr_type(ctx, hir_module, *rhs, hir_module.get_expression(*rhs));

            // For now, assume binary operations preserve types (lhs, rhs, and result are all the same type)
            // TODO: Add proper "behavior impl" constraints based on the operator
            ctx.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(*lhs)),
            );
            ctx.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(*rhs)),
            );
        }
        hir::Expression::Unit => {
            ctx.insert_type(expression_id, ResolvedType::Unit);
        }
        hir::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => {
            ctx.insert_expr_type_variable(expression_id);

            ctx.insert_type(*condition, ResolvedType::BuiltIn(hir::BuiltInType::Bool));
            collect_expr_type(ctx, hir_module, *then, hir_module.get_expression(*then));
            collect_expr_type(ctx, hir_module, *else_, hir_module.get_expression(*else_));
        }
        hir::Expression::Tuple(inners) => {
            ctx.add_expr_requirements(expression_id, TypeRequirements::Tuple(inners.clone()));
        }
        hir::Expression::Unary {
            op,
            expression: inner_id,
        } => {
            ctx.insert_expr_type_variable(expression_id);
            // todo: add "behavior impl" constraints based on the operator

            collect_expr_type(
                ctx,
                hir_module,
                *inner_id,
                hir_module.get_expression(*inner_id),
            );
        }
        hir::Expression::Lambda { args, body } => {
            // Collect type requirements for lambda arguments (patterns)
            for arg in args {
                collect_pattern_type(ctx, hir_module, *arg, hir_module.get_pattern(*arg));
            }

            // Collect type requirements for the body
            collect_expr_type(ctx, hir_module, *body, hir_module.get_expression(*body));

            // Add lambda constraint for this expression
            ctx.add_expr_requirements(
                expression_id,
                TypeRequirements::Lambda {
                    args: args.to_vec(),
                    body: *body,
                },
            );
        }
        hir::Expression::FunctionCall {
            target,
            scope,
            args,
        } => {
            ctx.insert_expr_type_variable(expression_id);

            // Collect types for all arguments
            for arg in args {
                collect_expr_type(ctx, hir_module, *arg, hir_module.get_expression(*arg));
            }

            // Look up the function being called
            match target {
                hir::Path::ThisModule(this_path) => {
                    // Try to find the function as an expression (lambda or function reference)
                    if let Some((func_id, func_expr)) =
                        hir_module.get_expression_by_name(this_path.first(), *scope)
                    {
                        collect_expr_type(ctx, hir_module, func_id, func_expr);

                        // If the function is a lambda, create bidirectional constraints
                        if let hir::Expression::Lambda {
                            args: lambda_args, ..
                        } = func_expr
                        {
                            // Link each call argument to the corresponding lambda parameter (bidirectional)
                            for (call_arg, lambda_param) in args.iter().zip(lambda_args.iter()) {
                                ctx.add_expr_requirements(
                                    *call_arg,
                                    TypeRequirements::MustBeSameAs(
                                        ExpressionOrPatternIdx::Pattern(*lambda_param),
                                    ),
                                );
                                ctx.add_pattern_requirements(
                                    *lambda_param,
                                    TypeRequirements::MustBeSameAs(
                                        ExpressionOrPatternIdx::Expression(*call_arg),
                                    ),
                                );
                            }
                        }

                        // Add a FunctionCall constraint that will be resolved during unification
                        ctx.add_expr_requirements(
                            expression_id,
                            TypeRequirements::FunctionCall {
                                func: ExpressionOrPatternIdx::Expression(func_id),
                                args: args.clone(),
                            },
                        );
                    } else if let Some((pattern_id, _)) =
                        hir_module.get_pattern_by_name(this_path.first(), *scope)
                    {
                        collect_pattern_type(
                            ctx,
                            hir_module,
                            pattern_id,
                            hir_module.get_pattern(pattern_id),
                        );

                        // Try to find what expression this pattern is bound to
                        // For "let x = |a, b| -> ...", we need to find the lambda
                        if let Some((bound_expr_id, bound_expr)) =
                            hir_module.get_expression_by_name(this_path.first(), *scope)
                        {
                            if let hir::Expression::Lambda {
                                args: lambda_args, ..
                            } = bound_expr
                            {
                                // Link each call argument to the corresponding lambda parameter (bidirectional)
                                for (call_arg, lambda_param) in args.iter().zip(lambda_args.iter())
                                {
                                    ctx.add_expr_requirements(
                                        *call_arg,
                                        TypeRequirements::MustBeSameAs(
                                            ExpressionOrPatternIdx::Pattern(*lambda_param),
                                        ),
                                    );
                                    ctx.add_pattern_requirements(
                                        *lambda_param,
                                        TypeRequirements::MustBeSameAs(
                                            ExpressionOrPatternIdx::Expression(*call_arg),
                                        ),
                                    );
                                }
                            }
                        }

                        // The pattern might be bound to a lambda
                        ctx.add_expr_requirements(
                            expression_id,
                            TypeRequirements::FunctionCall {
                                func: ExpressionOrPatternIdx::Pattern(pattern_id),
                                args: args.clone(),
                            },
                        );
                    }
                }
                hir::Path::OtherModule(_) => todo!("function call to other module"),
                hir::Path::Unknown(_) => {
                    // Unknown function - just give it a type variable
                }
            }
        }
        hir::Expression::Match { .. } => {
            todo!("match")
        }
    }
}

fn collect_pattern_type(
    ctx: &mut InferenceContext,
    hir_module: &hir::HirModule,
    pattern_id: hir::PatternIdx,
    pattern: &hir::Pattern,
) {
    println!("Collecting pattern type: {pattern:?}. id: {pattern_id:?}");
    match pattern {
        hir::Pattern::Missing => todo!("Missing pattern"),
        hir::Pattern::Literal(lit) => {
            ctx.insert_pattern_literal_type(pattern_id, lit);
        }
        hir::Pattern::PatternRef { path, scope } => {
            ctx.insert_pattern_type_variable(pattern_id);
            match path {
                hir::Path::ThisModule(this_path) => {
                    match hir_module.get_type_reference_by_name(this_path.first(), *scope) {
                        None => {
                            println!("No type annotation for pattern: {pattern:?}. id: {pattern_id:?}. path: {path:?}");
                        }
                        Some((type_id, _)) => {
                            ctx.add_pattern_requirements(
                                pattern_id,
                                TypeRequirements::Annotated(path.clone(), type_id),
                            );
                        }
                    };
                    match hir_module.get_expression_by_name(this_path.first(), *scope) {
                        None => {
                            match hir_module.get_pattern_by_name(this_path.first(), *scope) {
                                None => {
                                    println!("No expression or pattern for path: {pattern:?}. id: {pattern_id:?}. path: {path:?}");
                                }
                                Some((ref_id, _)) => {
                                    collect_pattern_type(
                                        ctx,
                                        hir_module,
                                        ref_id,
                                        hir_module.get_pattern(ref_id),
                                    );
                                    ctx.add_pattern_requirements(
                                        pattern_id,
                                        TypeRequirements::MustBeSameAs(
                                            ExpressionOrPatternIdx::Pattern(ref_id),
                                        ),
                                    );
                                }
                            };
                        }
                        Some((ref_id, _)) => {
                            collect_expr_type(
                                ctx,
                                hir_module,
                                ref_id,
                                hir_module.get_expression(ref_id),
                            );
                            ctx.add_pattern_requirements(
                                pattern_id,
                                TypeRequirements::MustBeSameAs(ExpressionOrPatternIdx::Expression(
                                    ref_id,
                                )),
                            );
                        }
                    };
                }
                hir::Path::OtherModule(_) => todo!("unknown path"),
                hir::Path::Unknown(_) => todo!("unknown path"),
            }
        }
        hir::Pattern::VariableDeclaration { .. } => {
            // Variable declarations get a type variable
            ctx.insert_pattern_type_variable(pattern_id);
        }
        hir::Pattern::Nil => {
            // Nil has a specific type
            ctx.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::Unit),
            );
        }
        hir::Pattern::Destructure { .. } => {
            // TODO: Handle destructuring patterns
            ctx.insert_pattern_type_variable(pattern_id);
        }
        hir::Pattern::Unit => {
            ctx.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::Unit),
            );
        }
        hir::Pattern::Tuple(_) => {
            // TODO: Handle tuple patterns
            ctx.insert_pattern_type_variable(pattern_id);
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
                .map(|inner_id| unify(ctx, ExpressionOrPatternIdx::Expression(*inner_id)))
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
        let body_type = unify(ctx, ExpressionOrPatternIdx::Expression(*body));

        // Work backwards through arguments to build nested lambda types
        let mut result_type = body_type;
        for arg in args.iter().rev() {
            let arg_type = unify(ctx, ExpressionOrPatternIdx::Pattern(*arg));
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
        let mut func_type = unify(ctx, *func);

        // Apply each argument to unwrap the curried lambda type
        for arg in args {
            match func_type {
                ResolvedType::Lambda {
                    arg_type: _,
                    return_type,
                } => {
                    // The argument type must match the parameter type (bidirectional constraint)
                    // This happens automatically through unification
                    let _arg_ty = unify(ctx, ExpressionOrPatternIdx::Expression(*arg));

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
        return unify(ctx, *other_id);
    }

    // Priority 5: Type annotations
    if let Some(TypeRequirements::Annotated(_, _)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Annotated(_, _)))
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
    if let Some(TypeRequirements::Variable(var_id)) = constraints
        .iter()
        .find(|c| matches!(c, TypeRequirements::Variable(_)))
    {
        return ResolvedType::Unknown;
    }

    ResolvedType::Unknown
}
