use alloy_hir as hir;
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

#[derive(Debug)]
pub struct InferenceResult {
    type_map: FxHashMap<hir::ExpressionIdx, ResolvedType>,
}

impl InferenceResult {
    fn new() -> Self {
        Self {
            type_map: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum TypeRequirements {
    MustBeType(ResolvedType),
    Tuple(NonEmpty<hir::ExpressionIdx>),
    MustBeSameAs(ExpressionOrPatternIdx),
    MustImplementTrait(hir::Path),
    Annotated(hir::Path, hir::TypeIdx),
    Variable(usize),
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
pub fn infer_types(hir_module: &hir::HirModule) -> InferenceResult {
    let mut ctx = InferenceContext::new();
    let mut result = InferenceResult::new();

    for (expression_id, expression, _range, name_op) in hir_module.expressions() {
        collect_expr_type(&mut ctx, hir_module, expression_id, expression);

        result.type_map.insert(
            expression_id,
            unify(&ctx, ExpressionOrPatternIdx::Expression(expression_id)),
        );
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
        hir::Expression::Lambda { .. } => {
            todo!("lambda")
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
        hir::Pattern::VariableDeclaration { .. } => {}
        hir::Pattern::Nil => {}
        hir::Pattern::Destructure { .. } => {}
        hir::Pattern::Unit => {}
        hir::Pattern::Tuple(_) => {}
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

    // we can only have a single type annotation
    let annotation = constraints.iter().find_map(|req| {
        if let TypeRequirements::Annotated(path, ty_id) = req {
            Some((path, ty_id))
        } else {
            None
        }
    });

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

    match must_be_types.len() {
        0 => {
            // handled below
        }
        1 => {
            return must_be_types[0].clone();
        }
        _ => {
            panic!("more than one 'MustBe' constraint");
        }
    }

    for constraint in &constraints {
        match constraint {
            TypeRequirements::MustBeType(ty) => {
                // handled above
            }
            TypeRequirements::MustBeSameAs(other_id) => {
                return unify(ctx, *other_id);
            }
            TypeRequirements::MustImplementTrait(_) => {
                todo!("trait impl");
            }
            TypeRequirements::Annotated(_, _) => {
                // handled above
            }
            TypeRequirements::Variable(_) => {
                // todo!("variable {:?}\n", constraints);
            }
            TypeRequirements::Tuple(inners) => unsafe {
                let inner_types = inners
                    .into_iter()
                    .map(|inner_id| unify(ctx, ExpressionOrPatternIdx::Expression(*inner_id)))
                    .collect();

                return ResolvedType::Tuple(NonEmpty::new_unchecked(inner_types));
            },
        }
    }

    ResolvedType::Unknown
}

#[cfg(test)]
mod tests {
    use alloy_hir as hir;
    use alloy_hir::ExpressionIdx;
    use alloy_parser::ParseError;
    use alloy_scope::ScopeIdx;
    use la_arena::RawIdx;
    use non_empty_vec::NonEmpty;

    use crate::tests::infer_types_repl_line;
    use crate::{InferenceResult, ResolvedType};

    fn check(
        (ctx, _hir_module, parse_errors): &(InferenceResult, hir::HirModule, Vec<ParseError>),
        expected: &[(u32, ResolvedType)],
    ) {
        assert_eq!(parse_errors, &[]);

        let expected = expected
            .into_iter()
            .map(|(id, ty)| (ExpressionIdx::from_raw(RawIdx::from(*id)), ty.clone()))
            .collect();

        assert_eq!(ctx.type_map, expected);
    }

    fn check_named(
        (ctx, hir_module, parse_errors): &(InferenceResult, hir::HirModule, Vec<ParseError>),
        expected: &[(&str, u32, ResolvedType)],
    ) {
        assert_eq!(parse_errors, &[]);

        let actual = expected
            .iter()
            .map(|(name, scope, _ty)| {
                let (expression_id, expression) = hir_module
                    .get_expression_by_name(
                        &hir::Name::new(*name),
                        ScopeIdx::from_raw(RawIdx::from(*scope)),
                    )
                    .expect("expression not found");
                (*name, *scope, ctx.type_map[&expression_id].clone())
            })
            .collect::<Vec<_>>();

        assert_eq!(actual, expected);
    }

    #[test]
    fn infer_literals() {
        check(
            &infer_types_repl_line("1"),
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Int))],
        );
        check(
            &infer_types_repl_line("1.1"),
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Fraction))],
        );
        check(
            &infer_types_repl_line(r#""hello""#),
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::String))],
        );
        check(
            &infer_types_repl_line("'c'"),
            &[(0, ResolvedType::BuiltIn(hir::BuiltInType::Char))],
        );
    }

    #[test]
    fn infer_variable_ref_literal() {
        check_named(
            &infer_types_repl_line(
                r"
                let x = 1
                let y = x
            ",
            ),
            &[("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int))],
        );
    }

    #[test]
    fn infer_variable_ref_tuple() {
        unsafe {
            check_named(
                &infer_types_repl_line(
                    r#"
                let x = 1
                let y = "a"
                let z = (x, y)
            "#,
                ),
                &[
                    ("x", 0, ResolvedType::BuiltIn(hir::BuiltInType::Int)),
                    ("y", 0, ResolvedType::BuiltIn(hir::BuiltInType::String)),
                    (
                        "z",
                        0,
                        ResolvedType::Tuple(NonEmpty::new_unchecked(vec![
                            ResolvedType::BuiltIn(hir::BuiltInType::Int),
                            ResolvedType::BuiltIn(hir::BuiltInType::String),
                        ])),
                    ),
                ],
            );
        }
    }

    #[test]
    fn infer_variable_ref_unused_lambda() {
        check_named(
            &infer_types_repl_line("let x = |a, b| => a + b"),
            &[(
                "x",
                0,
                ResolvedType::Lambda {
                    arg_type: Box::new(ResolvedType::TypeVar(0)),
                    return_type: Box::new(ResolvedType::Lambda {
                        arg_type: Box::new(ResolvedType::TypeVar(1)),
                        return_type: Box::new(ResolvedType::TypeVar(2)),
                    }),
                },
            )],
        );
    }
}
