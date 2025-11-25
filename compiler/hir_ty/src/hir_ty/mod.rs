use crate::HirTypedModule;
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;
use std::collections::HashSet;

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
    TypeVar(usize),
}

#[derive(Debug)]
pub struct InferenceContext {
    type_requirements: FxHashMap<ExpressionOrPatternIdx, HashSet<TypeRequirements>>,
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
        match lit {
            hir::Literal::Int(_) => self.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
            ),
            hir::Literal::Fraction(_) => self.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Fraction)),
            ),
            hir::Literal::String(_) => self.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::String)),
            ),
            hir::Literal::Char(_) => self.add_expr_requirements(
                expression_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Char)),
            ),
        }
    }

    fn insert_pattern_literal_type(&mut self, pattern_id: hir::PatternIdx, lit: &hir::Literal) {
        match lit {
            hir::Literal::Int(_) => self.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Int)),
            ),
            hir::Literal::Fraction(_) => self.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Fraction)),
            ),
            hir::Literal::String(_) => self.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::String)),
            ),
            hir::Literal::Char(_) => self.add_pattern_requirements(
                pattern_id,
                TypeRequirements::MustBeType(ResolvedType::BuiltIn(hir::BuiltInType::Char)),
            ),
        }
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
        self.type_requirements
            .entry(ExpressionOrPatternIdx::Expression(expression_id))
            .or_insert_with(HashSet::new)
            .insert(constraint);
    }

    fn add_pattern_requirements(
        &mut self,
        pattern_id: hir::PatternIdx,
        constraint: TypeRequirements,
    ) {
        self.type_requirements
            .entry(ExpressionOrPatternIdx::Pattern(pattern_id))
            .or_insert_with(HashSet::new)
            .insert(constraint);
    }
}

#[must_use]
pub fn infer_types(db: &dyn crate::HirTyDatabase, module_id: ModuleId) -> HirTypedModule {
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
    for (expression_id, _expression, _range, _name_op) in hir_module.expressions() {
        if ctx
            .type_requirements
            .contains_key(&ExpressionOrPatternIdx::Expression(expression_id))
        {
            let resolved_type = unify(&ctx, ExpressionOrPatternIdx::Expression(expression_id));
            result.expression_types.insert(expression_id, resolved_type);
        }
    }

    // Unify and resolve types for all patterns
    for (pattern_id, _pattern, _range, _name_op) in hir_module.patterns() {
        if ctx
            .type_requirements
            .contains_key(&ExpressionOrPatternIdx::Pattern(pattern_id))
        {
            let resolved_type = unify(&ctx, ExpressionOrPatternIdx::Pattern(pattern_id));
            result.pattern_types.insert(pattern_id, resolved_type);
        }
    }

    result
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
            // todo: add "behavior impl" constraints based on the operator
            // match op {
            //     hir::BinaryOp::Add => {
            //         ctx.insert(expression_id, ResolvedType::BuiltIn(hir::BuiltInType::Int));
            //     }
            //     hir::BinaryOp::Sub => {
            //         ctx.insert(expression_id, ResolvedType::BuiltIn(hir::BuiltInType::Int));
            //     }
            //     hir::BinaryOp::Mul => {
            //         ctx.insert(expression_id, ResolvedType::BuiltIn(hir::BuiltInType::Int));
            //     }
            //     hir::BinaryOp::Div => {
            //         ctx.insert(expression_id, ResolvedType::BuiltIn(hir::BuiltInType::Int));
            //     }
            //     hir::BinaryOp::Custom(_) => {
            //         todo!("custom binary op");
            //     }
            //     hir::BinaryOp::Missing => {
            //         todo!("missing binary op");
            //     }
            // }
            ctx.insert_expr_type_variable(expression_id);

            collect_expr_type(ctx, hir_module, *lhs, hir_module.get_expression(*lhs));
            collect_expr_type(ctx, hir_module, *rhs, hir_module.get_expression(*rhs));
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
        hir::Expression::FunctionCall { .. } => {
            todo!("function call")
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
        // TODO: Handle type annotations
        // For now, fall through to TypeVar
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
        return ResolvedType::TypeVar(*var_id);
    }

    ResolvedType::Unknown
}
