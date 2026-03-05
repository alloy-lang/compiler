use crate::resolver::resolve_by_path;
use crate::{Fql, TypeVariableResolver};
use alloy_hir_def as hir;
use alloy_hir_def::HirDefDatabase;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnnotatedType {
    Unit,
    BuiltIn(hir::BuiltInType),
    /// Concrete named type (Single/Union type definition)
    TypeDef {
        fql: Fql<hir::TypeDefinition>,
        name: hir::Name,
        type_args: Vec<TypeVarReference>,
    },
    /// Function type
    Lambda {
        arg: Box<AnnotatedType>,
        ret: Box<AnnotatedType>,
    },
    /// Tuple type
    Tuple(NonEmpty<AnnotatedType>),
    /// Parameterized type (e.g., List[Int])
    Bounded {
        base: Box<AnnotatedType>,
        args: Vec<AnnotatedType>,
    },
    /// Unconstrained type variable (from typevar declaration)
    TypeVar {
        fql: Fql<hir::TypeVariable>,
        name: hir::Name,
    },
    /// Constrained type variable (typevar with trait bounds)
    ConstrainedTypeVar {
        fql: Fql<hir::TypeVariable>,
        name: hir::Name,
        constraints: NonEmpty<(Fql<hir::Trait>, hir::Name)>,
    },
    /// Self type in a trait context
    SelfType {
        trait_fql: Fql<hir::Trait>,
        type_arity: usize,
        trait_constraints: Vec<(Fql<hir::Trait>, hir::Name)>,
    },
    /// Explicitly unconstrained (wildcard)
    Unconstrained,
    /// Syntax error or unresolvable
    Missing,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarReference {
    pub fql: Fql<hir::TypeVariable>,
    pub name: hir::Name,
}

impl AnnotatedType {
    /// Check if this type contains type variables (is polymorphic)
    pub fn is_polymorphic(&self) -> bool {
        match self {
            AnnotatedType::TypeVar { .. }
            | AnnotatedType::ConstrainedTypeVar { .. }
            | AnnotatedType::SelfType { .. } => true,
            AnnotatedType::Lambda { arg, ret } => arg.is_polymorphic() || ret.is_polymorphic(),
            AnnotatedType::Tuple(elements) => elements.iter().any(|e| e.is_polymorphic()),
            AnnotatedType::Bounded { base, args } => {
                base.is_polymorphic() || args.iter().any(|a| a.is_polymorphic())
            }
            AnnotatedType::Unit
            | AnnotatedType::BuiltIn(_)
            | AnnotatedType::TypeDef { .. }
            | AnnotatedType::Unconstrained
            | AnnotatedType::Missing => false,
        }
    }
}

impl std::fmt::Display for AnnotatedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotatedType::Unit => write!(f, "()"),
            AnnotatedType::BuiltIn(builtin) => write!(f, "{builtin:?}"),
            AnnotatedType::TypeDef { name, .. } => write!(f, "{name}"),
            AnnotatedType::Lambda { arg, ret } => match arg.as_ref() {
                AnnotatedType::Lambda { .. } => write!(f, "({arg}) -> {ret}"),
                _ => write!(f, "{arg} -> {ret}"),
            },
            AnnotatedType::Tuple(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            AnnotatedType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, "]")
            }
            AnnotatedType::TypeVar { name, .. } => write!(f, "{name}"),
            AnnotatedType::ConstrainedTypeVar {
                name, constraints, ..
            } => {
                write!(f, "{name}")?;
                write!(f, " : ")?;
                for (i, (_, trait_name)) in constraints.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{trait_name}")?;
                }
                Ok(())
            }
            AnnotatedType::SelfType {
                trait_constraints: constraints,
                ..
            } => {
                write!(f, "Self")?;
                if !constraints.is_empty() {
                    write!(f, " : ")?;
                    for (i, (_, trait_name)) in constraints.iter().enumerate() {
                        if i > 0 {
                            write!(f, " + ")?;
                        }
                        write!(f, "{trait_name}")?;
                    }
                }
                Ok(())
            }
            AnnotatedType::Unconstrained => write!(f, "_"),
            AnnotatedType::Missing => write!(f, "<missing>"),
        }
    }
}

//
// resolve
//

#[salsa::tracked]
pub fn resolve_annotated_type(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Unconstrained => AnnotatedType::Unconstrained,
        hir::TypeReference::Missing => AnnotatedType::Missing,
        hir::TypeReference::Unit => AnnotatedType::Unit,
        hir::TypeReference::BuiltIn(built_in) => AnnotatedType::BuiltIn(*built_in),

        hir::TypeReference::SelfRef(scope) => {
            // In a behavior, Self resolves to the concrete attached_type
            if let Some((_idx, behavior)) = hir_module.find_behavior_containing_scope(*scope) {
                resolve_annotated_type(db, module_id, behavior.attached_type)
            }
            // In a trait, Self is a polymorphic type variable
            else if let Some((trait_idx, trait_def)) =
                hir_module.find_trait_containing_scope(*scope)
            {
                let trait_fql = Fql::new(module_id, trait_idx);
                let trait_constraints: Vec<_> = trait_def
                    .self_constraints()
                    .iter()
                    .filter_map(|constraint| trait_constraints(db, module_id, constraint))
                    .collect();
                let kind_constraints = trait_def
                    .self_constraints()
                    .iter()
                    .find_map(kind_constraints);

                AnnotatedType::SelfType {
                    trait_fql,
                    type_arity: kind_constraints.unwrap_or(0),
                    trait_constraints,
                }
            } else {
                // Self used outside trait/behavior context
                AnnotatedType::Missing
            }
        }

        hir::TypeReference::Named(path) => {
            resolve_named_type_annotation(db, module_id, path, type_idx)
        }

        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            let arg = resolve_annotated_type(db, module_id, *arg_type);
            let ret = resolve_annotated_type(db, module_id, *return_type);
            AnnotatedType::Lambda {
                arg: Box::new(arg),
                ret: Box::new(ret),
            }
        }

        hir::TypeReference::Tuple(types) => {
            if types.is_empty() {
                AnnotatedType::Unit
            } else {
                let inner_types: Vec<_> = types
                    .iter()
                    .map(|t| resolve_annotated_type(db, module_id, *t))
                    .collect();
                // SAFETY: We checked that types is non-empty
                unsafe { AnnotatedType::Tuple(NonEmpty::new_unchecked(inner_types)) }
            }
        }

        hir::TypeReference::ParenthesizedType(inner) => {
            resolve_annotated_type(db, module_id, *inner)
        }

        hir::TypeReference::Bounded { base, args } => {
            let base_resolved = resolve_annotated_type(db, module_id, *base);
            let args_resolved: Vec<_> = args
                .iter()
                .map(|arg| resolve_annotated_type(db, module_id, *arg))
                .collect();

            AnnotatedType::Bounded {
                base: Box::new(base_resolved),
                args: args_resolved,
            }
        }
    }
}

#[salsa::tracked]
pub fn resolve_annotated_expression(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
    expr_idx: hir::ExpressionIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let expr = hir_module.get_expression(expr_idx);

    resolve_annotated_expression_inner(db, module_id, type_idx, expr)
}

fn resolve_annotated_expression_inner(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
    expr: &hir::Expression,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match (type_ref, expr) {
        (_, hir::Expression::VariableRef { .. }) => {
            return resolve_annotated_type(db, module_id, type_idx);
        }
        (hir::TypeReference::Unconstrained, _) => {
            return AnnotatedType::Unconstrained;
        }
        (hir::TypeReference::Missing, _) => {
            return AnnotatedType::Missing;
        }
        (hir::TypeReference::Unit, _) => {
            return AnnotatedType::Unit;
        }
        (hir::TypeReference::BuiltIn(built_in), _) => {
            return AnnotatedType::BuiltIn(*built_in);
        }
        (hir::TypeReference::SelfRef(_), _) => {
            todo!("self type annotations on expressions - need to determine context (trait/behavior) to resolve properly");
        }

        (hir::TypeReference::Named(path), _) => {
            return resolve_named_type_annotation(db, module_id, path, type_idx);
        }

        (
            hir::TypeReference::Lambda {
                arg_type,
                return_type,
            },
            hir::Expression::Lambda { args, body },
        ) => {
            return if let [first, rest @ ..] = args.as_slice() {
                let annotated_arg_type =
                    resolve_annotated_pattern(db, module_id, *arg_type, *first);
                if rest.is_empty() {
                    return AnnotatedType::Lambda {
                        arg: Box::new(annotated_arg_type),
                        ret: Box::new(resolve_annotated_expression(
                            db,
                            module_id,
                            *return_type,
                            *body,
                        )),
                    };
                }

                let remainder_expr = hir::Expression::Lambda {
                    args: rest.into(),
                    body: *body,
                };
                let ret_type = resolve_annotated_expression_inner(
                    db,
                    module_id,
                    *return_type,
                    &remainder_expr,
                );

                AnnotatedType::Lambda {
                    arg: Box::new(annotated_arg_type),
                    ret: Box::new(ret_type),
                }
            } else {
                // Lambda with no arguments - this is not valid syntax, but we'll return Missing to avoid panicking
                AnnotatedType::Missing
            };
        }

        (hir::TypeReference::Tuple(types), hir::Expression::Tuple(exprs)) => {
            if types.len() != exprs.len().get() {
                // Mismatched tuple arity - report error and return Missing
                return AnnotatedType::Missing;
            }

            let inner_types: Vec<_> = types
                .iter()
                .zip(exprs.iter())
                .map(|(type_idx, expr_idx)| {
                    resolve_annotated_expression(db, module_id, *type_idx, *expr_idx)
                })
                .collect();

            // SAFETY: We checked that types is non-empty
            return unsafe { AnnotatedType::Tuple(NonEmpty::new_unchecked(inner_types)) };
        }

        (hir::TypeReference::ParenthesizedType(inner_type), _expr) => {
            return resolve_annotated_expression_inner(db, module_id, *inner_type, expr);
        }

        (hir::TypeReference::Bounded { .. }, _) => {
            return resolve_annotated_type(db, module_id, type_idx);
        }
        _ => {
            // For other combinations of type reference and expression, we currently don't have specific handling logic.
            // In a full implementation, we would likely want to add more cases here to handle different expression forms and how they interact with their annotated types.
        }
    }

    todo!("Unhandled combination of type reference and expression: {type_ref:?} with {expr:?}");
}

#[salsa::tracked]
pub fn resolve_annotated_pattern(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
    pattern_idx: hir::PatternIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);
    let pattern = hir_module.get_pattern(pattern_idx);

    match (type_ref, pattern) {
        (_, hir::Pattern::Nil) => {
            return resolve_annotated_type(db, module_id, type_idx);
        }
        (hir::TypeReference::Unconstrained, _) => {
            return AnnotatedType::Unconstrained;
        }
        (hir::TypeReference::Missing, _) => {
            return AnnotatedType::Missing;
        }
        (hir::TypeReference::Unit, _) => {
            return AnnotatedType::Unit;
        }
        (hir::TypeReference::BuiltIn(built_in), _) => {
            return AnnotatedType::BuiltIn(*built_in);
        }
        (hir::TypeReference::SelfRef(scope), _) => {
            todo!("");
        }

        (hir::TypeReference::Named(path), _) => {
            return resolve_named_type_annotation(db, module_id, path, type_idx);
        }

        (hir::TypeReference::Lambda { .. }, hir::Pattern::VariableDeclaration { .. }) => {
            return resolve_annotated_type(db, module_id, type_idx);
        }

        (hir::TypeReference::Tuple(types), hir::Pattern::Tuple(patterns)) => {
            if types.len() != patterns.len().get() {
                // Mismatched tuple arity - report error and return Missing
                return AnnotatedType::Missing;
            }

            let inner_types: Vec<_> = types
                .iter()
                .zip(patterns.iter())
                .map(|(type_idx, p_idx)| {
                    resolve_annotated_pattern(db, module_id, *type_idx, *p_idx)
                })
                .collect();

            // SAFETY: We checked that types is non-empty
            return unsafe { AnnotatedType::Tuple(NonEmpty::new_unchecked(inner_types)) };
        }

        (hir::TypeReference::ParenthesizedType(inner_type), _pattern) => {
            return resolve_annotated_pattern(db, module_id, *inner_type, pattern_idx);
        }

        (hir::TypeReference::Bounded { .. }, _) => {
            return resolve_annotated_type(db, module_id, type_idx);
        }
        _ => {
            // For other combinations of type reference and pattern, we currently don't have specific handling logic.
            // In a full implementation, we would likely want to add more cases here to handle different pattern forms and how they interact with their annotated types.
        }
    }

    todo!("Unhandled combination of type reference and pattern: {type_ref:?} with {pattern:?}");
}

fn trait_constraints(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    c: &hir::TypeVariableConstraint,
) -> Option<(Fql<hir::Trait>, hir::Name)> {
    match c {
        hir::TypeVariableConstraint::Trait(type_idx) => {
            let trait_fql = crate::resolve_trait_by_ref_id(db, module_id, *type_idx).ok()?;
            let name = trait_fql.trait_name(db);
            Some((trait_fql, name))
        }
        hir::TypeVariableConstraint::Kind(_) => None,
    }
}

fn kind_constraints(c: &hir::TypeVariableConstraint) -> Option<usize> {
    match c {
        hir::TypeVariableConstraint::Trait(_) => None,
        hir::TypeVariableConstraint::Kind(arity) => Some(*arity),
    }
}

/// Resolve a Named type reference path to an AnnotatedType.
///
/// First tries to resolve as a type reference (finds TypeReference in target module),
/// then falls back to resolving as a type definition (TypeDefinition directly).
fn resolve_named_type_annotation(
    db: &dyn HirDefDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    type_idx: hir::TypeIdx,
) -> AnnotatedType {
    let source_fql = Fql::new(current_module_id, type_idx);

    // First try: resolve via type reference path
    if let Some(resolved_fql) =
        crate::resolve_type_reference_by_path(db, current_module_id, path, &source_fql)
    {
        return resolve_annotated_type(db, resolved_fql.module_id, resolved_fql.local_id);
    }

    // Second try: resolve directly as a type definition
    if let Ok(td_fql) = crate::resolve_type_definition_by_ref_id(db, current_module_id, type_idx) {
        return resolve_type_definition_to_annotated(db, td_fql.module_id, td_fql.local_id);
    }

    if let Ok(tv_fql) = resolve_by_path::<hir::TypeVariable, TypeVariableResolver>(
        db,
        current_module_id,
        path,
        &source_fql,
    ) {
        return resolve_type_variable_to_annotated(db, tv_fql.module_id, tv_fql.local_id);
    }

    AnnotatedType::Missing
}

#[salsa::tracked]
pub fn resolve_type_definition_to_annotated(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_def_idx: hir::TypeDefinitionIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let hir::TypeDefinition {
        name,
        kind,
        type_args,
    } = hir_module.get_type_definition(type_def_idx);

    match kind {
        hir::TypeDefinitionKind::Missing => AnnotatedType::Missing,
        hir::TypeDefinitionKind::Single(_) | hir::TypeDefinitionKind::Union(_) => {
            AnnotatedType::TypeDef {
                fql: Fql::new(module_id, type_def_idx),
                type_args: type_args
                    .iter()
                    .map(|arg| TypeVarReference {
                        fql: Fql::new(module_id, *arg),
                        name: hir_module.get_type_variable(*arg).name.clone(),
                    })
                    .collect(),
                name: name.clone(),
            }
        }
    }
}

#[salsa::tracked]
pub fn resolve_type_variable_to_annotated(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    type_var_idx: hir::TypeVariableIdx,
) -> AnnotatedType {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let hir::TypeVariable { name, kind } = hir_module.get_type_variable(type_var_idx);

    let fql = Fql::new(module_id, type_var_idx);
    match kind {
        hir::TypeVariableKind::Unbound => AnnotatedType::TypeVar {
            fql,
            name: name.clone(),
        },
        hir::TypeVariableKind::Constrained(constraints) => {
            let trait_constraints: Vec<_> = constraints
                .iter()
                .filter_map(|constraint| trait_constraints(db, module_id, constraint))
                .collect();

            NonEmpty::try_from(trait_constraints)
                .map(|constraints| AnnotatedType::ConstrainedTypeVar {
                    fql: fql.clone(),
                    name: name.clone(),
                    constraints,
                })
                .unwrap_or_else(|_| AnnotatedType::TypeVar {
                    fql,
                    name: name.clone(),
                })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
    use alloy_hir_def::BuiltInType;
    use alloy_test_harness::idx;
    use alloy_workspace::WorkspaceDatabase;
    use salsa::Database;
    use std::convert::TryFrom;

    #[test]
    fn resolve_literal_int() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let module_id = db.add_test_module(
            "test_stuff",
            r"
            typeof example : Int
            let example = 1
            ",
        );

        let actual_type = resolve_annotated_expression(&db, module_id, idx!(0), idx!(0));

        db.attach(|_| {
            assert_eq!(AnnotatedType::BuiltIn(hir::BuiltInType::Int), actual_type);
        });
    }

    #[test]
    fn resolve_lambda() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let test_data_module_id = db.add_test_module(
            "test_data",
            r"
            typedef Test[t] = Thing t
            let test = Test(0)
            let new = |t| -> Test(t)

            trait Trait1 where
                -- empty
            end
            ",
        );
        let module_id = db.add_test_module(
            "test_stuff",
            r"
            import test_data
            import test_data::Test
            import std::function::(<|)

            typeof example : (t2 -> t1) -> t2 -> t2 -> Test[(t1, t1)] where
              typevar t1
              typevar t2
            let example = |funky, x, y| -> test_data::new <| (funky(x), funky(y))
            ",
        );

        let actual_type = resolve_annotated_expression(&db, module_id, idx!(13), idx!(7));

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(AnnotatedType::TypeVar {
                            fql: Fql::new(module_id, idx!(1)),
                            name: hir::Name::from("t2"),
                        }),
                        ret: Box::new(AnnotatedType::TypeVar {
                            fql: Fql::new(module_id, idx!(0)),
                            name: hir::Name::from("t1"),
                        }),
                    }),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(AnnotatedType::TypeVar {
                            fql: Fql::new(module_id, idx!(1)),
                            name: hir::Name::from("t2"),
                        }),
                        ret: Box::new(AnnotatedType::Lambda {
                            arg: Box::new(AnnotatedType::TypeVar {
                                fql: Fql::new(module_id, idx!(1)),
                                name: hir::Name::from("t2"),
                            }),
                            ret: Box::new(AnnotatedType::Bounded {
                                base: Box::new(AnnotatedType::TypeDef {
                                    fql: Fql::new(test_data_module_id, idx!(0)),
                                    name: hir::Name::from("Test"),
                                    type_args: vec![TypeVarReference {
                                        fql: Fql {
                                            module_id: test_data_module_id,
                                            local_id: idx!(0),
                                        },
                                        name: hir::Name::from("t"),
                                    }],
                                }),
                                args: vec![AnnotatedType::Tuple(
                                    NonEmpty::try_from(vec![
                                        AnnotatedType::TypeVar {
                                            fql: Fql::new(module_id, idx!(0)),
                                            name: hir::Name::from("t1"),
                                        },
                                        AnnotatedType::TypeVar {
                                            fql: Fql::new(module_id, idx!(0)),
                                            name: hir::Name::from("t1"),
                                        },
                                    ])
                                    .unwrap()
                                )],
                            }),
                        }),
                    }),
                },
                actual_type,
            );
        });
    }

    #[test]
    fn infer_cross_module_typedef_in_lambda() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let test_data_module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            r"
            typedef Test[t] = Thing t
            let test = Test(0)
            ",
        );
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
            import test_data::test
            import test_data

            typeof f : t1 -> t2 -> test_data::Test[Int] where
              typevar t1
              typevar t2
            let f = |a, b| -> test
            ",
        );

        let actual_type = resolve_annotated_expression(&db, module_id, idx!(6), idx!(1));

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(AnnotatedType::TypeVar {
                        fql: Fql::new(module_id, idx!(0)),
                        name: hir::Name::from("t1"),
                    }),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(AnnotatedType::TypeVar {
                            fql: Fql::new(module_id, idx!(1)),
                            name: hir::Name::from("t2"),
                        }),
                        ret: Box::new(AnnotatedType::Bounded {
                            base: Box::new(AnnotatedType::TypeDef {
                                fql: Fql::new(test_data_module_id, idx!(0)),
                                name: hir::Name::from("Test"),
                                type_args: vec![TypeVarReference {
                                    fql: Fql {
                                        module_id: test_data_module_id,
                                        local_id: idx!(0),
                                    },
                                    name: hir::Name::from("t"),
                                }],
                            }),
                            args: vec![AnnotatedType::BuiltIn(BuiltInType::Int)],
                        }),
                    }),
                },
                actual_type,
            );
        });
    }
}
