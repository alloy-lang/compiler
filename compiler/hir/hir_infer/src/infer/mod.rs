use crate::{HirInferDatabase, InferredType};
use alloy_hir_def as hir;
use alloy_hir_def::{ExpressionIdx, PatternIdx, TypeIdx, TypeReference};
use alloy_hir_resolved as res;
use alloy_hir_resolved::{EPFql, Expression, Fql};
use alloy_workspace::ModuleId;
use itertools::{EitherOrBoth, Itertools};
use non_empty_vec::NonEmpty;
use res::Pattern;

/// A type equation for unification (e.g., `τ1 = τ2`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEquation {
    pub(super) left: InferredType,
    pub(super) right: InferredType,
    pub(super) source: EPFql,
}

#[salsa::tracked]
pub fn infer_expr(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
    idx: ExpressionIdx,
) -> Vec<TypeEquation> {
    let fql = Fql::new(module_id, idx);

    let expr = match res::resolve_expression_by_id(db, module_id, idx) {
        Ok(expr) => expr,
        Err(err) => {
            return vec![TypeEquation {
                left: InferredType::ResolutionError(err),
                right: InferredType::Variable(EPFql::Expression(fql)),
                source: EPFql::Expression(fql),
            }];
        }
    };

    let left = match expr {
        Expression::Missing => InferredType::Missing,
        Expression::Literal(lit) => InferredType::BuiltIn(hir::BuiltInType::from(&lit)),
        Expression::Unit => InferredType::Unit,
        Expression::VariableRef(ref_fql) => {
            return match ref_fql {
                EPFql::Expression(expr_fql) => {
                    infer_expr(db, expr_fql.module_id, expr_fql.local_id)
                }
                EPFql::Pattern(pat_fql) => infer_pattern(db, pat_fql.module_id, pat_fql.local_id),
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            let lhs_tys = infer_expr(db, lhs.module_id, lhs.local_id);
            let rhs_tys = infer_expr(db, rhs.module_id, rhs.local_id);

            match op {
                hir::BinaryOp::Add
                | hir::BinaryOp::Sub
                | hir::BinaryOp::Mul
                | hir::BinaryOp::Div => {
                    let num_ty = InferredType::Variable(EPFql::Expression(fql.clone()));
                    let lhs_ty = InferredType::Variable(EPFql::Expression(lhs));
                    let rhs_ty = InferredType::Variable(EPFql::Expression(rhs));

                    return vec![
                        vec![
                            TypeEquation {
                                left: lhs_ty,
                                right: num_ty.clone(),
                                source: EPFql::Expression(fql.clone()),
                            },
                            TypeEquation {
                                left: rhs_ty,
                                right: num_ty.clone(),
                                source: EPFql::Expression(fql.clone()),
                            },
                        ],
                        lhs_tys,
                        rhs_tys,
                    ]
                    .concat();
                }
                hir::BinaryOp::Custom(_) => {
                    unimplemented!("hir::BinaryOp::Custom")
                }
                hir::BinaryOp::Missing => {}
            }

            todo!()
        }
        Expression::Tuple(_) => {}
        Expression::IfThenElse { .. } => {}
        Expression::Unary { .. } => {}
        Expression::Lambda { .. } => {}
        Expression::FunctionCall { .. } => {}
        Expression::AbstractTraitFunctionCall { .. } => {}
        Expression::Match { .. } => {}
        Expression::DataConstructor { .. } => {}
        Expression::VariantConstructor { .. } => {}
        Expression::AbstractTraitMemberRef { .. } => {}
    };

    vec![TypeEquation {
        left,
        right: InferredType::Variable(EPFql::Expression(fql)),
        source: EPFql::Expression(fql),
    }]
}

#[salsa::tracked]
pub fn infer_pattern(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
    idx: PatternIdx,
) -> Vec<TypeEquation> {
    let fql = Fql::new(module_id, idx);

    let pattern = match alloy_hir_resolved::resolve_pattern_by_id(db, module_id, idx) {
        Ok(p) => p,
        Err(err) => {
            return vec![TypeEquation {
                left: InferredType::ResolutionError(err),
                right: InferredType::Variable(EPFql::Pattern(fql)),
                source: EPFql::Pattern(fql),
            }];
        }
    };

    let left = match pattern {
        Pattern::Missing => InferredType::Missing,
        Pattern::Literal(lit) => InferredType::BuiltIn(hir::BuiltInType::from(&lit)),
        Pattern::Unit => InferredType::Unit,
        Pattern::Nil => InferredType::Unconstrained,
        Pattern::VariableDeclaration => InferredType::Unconstrained,
        Pattern::Tuple(elements) => {
            let element_eqs = elements
                .iter()
                .map(|elem_id| infer_pattern(db, elem_id.module_id, elem_id.local_id))
                .collect();
            let element_types = elements
                .iter()
                .map(|elem_id| InferredType::Variable(EPFql::Pattern(*elem_id)))
                .collect();

            return vec![
                element_eqs,
                vec![TypeEquation {
                    left: InferredType::Tuple(unsafe { NonEmpty::new_unchecked(element_types) }),
                    right: InferredType::Variable(EPFql::Pattern(fql)),
                    source: EPFql::Pattern(fql),
                }],
            ]
            .concat();
        }
        Pattern::DataDestructure { target, args } => {
            let Some(type_def) =
                res::resolve_type_definition_by_id(db, target.module_id, target.local_id)
            else {
                return vec![TypeEquation {
                    left: InferredType::Missing,
                    right: InferredType::Variable(EPFql::Pattern(fql)),
                    source: EPFql::Pattern(fql),
                }];
            };

            let res::TypeDefinitionKind::Single(member) = type_def.kind else {
                unreachable!("we cannot hit a union type def here")
            };

            infer_type_def_member(db, &target, &args, &member)
        }
        Pattern::VariantDestructure {
            target,
            args,
            variant_name,
        } => {
            let Some(type_def) =
                res::resolve_type_definition_by_id(db, target.module_id, target.local_id)
            else {
                return InferredType::Missing;
            };

            let res::TypeDefinitionKind::Union(members) = type_def.kind else {
                unreachable!("we cannot hit a union type def here")
            };
            let member = members
                .iter()
                .find(|m| m.name() == &variant_name)
                .expect("we must find a type def member");

            infer_type_def_member(db, &target, &args, member)
        }
    };

    vec![TypeEquation {
        left,
        right: InferredType::Variable(EPFql::Pattern(fql)),
        source: EPFql::Pattern(fql),
    }]
}

pub fn infer_type_def_member(
    db: &dyn HirInferDatabase,
    target: &Fql<hir::TypeDefinition>,
    args: &[Fql<hir::Pattern>],
    member: &res::TypeDefinitionMember,
) -> Vec<TypeEquation> {
    let mut eqs = Vec::new();

    for either in args.iter().zip_longest(member.properties()) {
        match either {
            EitherOrBoth::Both(arg, property) => {
                let ty = res::resolve_annotated_type(db, property.module_id, property.local_id);
                InferredType::Annotated(ty);
            }
            EitherOrBoth::Left(arg) => {}
            EitherOrBoth::Right(property) => {}
        }
    }
}

#[salsa::tracked]
pub fn infer_type_ref(
    db: &dyn HirInferDatabase,
    module_id: ModuleId,
    idx: TypeIdx,
) -> InferredType {
    let ty = res::resolve_annotated_type(db, module_id, idx);
    InferredType::Annotated(ty)
}
