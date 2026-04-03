use crate::resolver::resolve_by_path;
use crate::{resolve_behavior_by_id, Fql, TypeVariableResolver};
use alloy_hir_def as hir;
use alloy_hir_def::HirDefDatabase;
use alloy_workspace::ModuleId;
use itertools::Itertools;
use non_empty_vec::NonEmpty;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitConstraint {
    pub trait_fql: Fql<hir::Trait>,
    pub trait_name: hir::Name,
}

impl TraitConstraint {
    pub fn has_behavior_for_trait(
        &self,
        db: &dyn HirDefDatabase,
        type_fql: &Fql<hir::TypeDefinition>,
    ) -> bool {
        let (hir_module, _) = hir::lower_file(db, type_fql.module_id);
        for (behavior_idx, _, _, _) in hir_module.behaviors() {
            let behavior = resolve_behavior_by_id(db, type_fql.module_id, behavior_idx);
            let Ok(attached_type) = &behavior.attached_type else {
                continue;
            };
            let Ok(attached_trait) = &behavior.attached_trait else {
                continue;
            };
            if attached_type == type_fql && attached_trait == &self.trait_fql {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotatedTypeVar {
    pub fql: Fql<hir::TypeVariable>,
    pub name: hir::Name,
    pub type_arity: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnnotatedType {
    Unit,
    BuiltIn(hir::BuiltInType),
    /// Concrete named type (Single/Union type definition)
    /// TODO: bounded vs unbounded type defs
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
    /// TODO: bounded vs unbounded type defs
    Bounded {
        base: Box<AnnotatedType>,
        args: Vec<AnnotatedType>,
    },
    /// Unconstrained type variable (from typevar declaration)
    TypeVar(AnnotatedTypeVar),
    /// Constrained type variable (typevar with trait bounds)
    ConstrainedTypeVar {
        base: AnnotatedTypeVar,
        constraints: NonEmpty<TraitConstraint>,
    },
    /// Self type in a trait context
    SelfType {
        trait_fql: Fql<hir::Trait>,
        type_arity: usize,
        trait_constraints: Vec<TraitConstraint>,
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
    #[must_use]
    pub fn is_polymorphic(&self) -> bool {
        match self {
            AnnotatedType::TypeVar(_)
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

    pub fn type_arity(&self) -> usize {
        match self {
            AnnotatedType::TypeDef { type_args, .. } => type_args.len(),
            AnnotatedType::BuiltIn(_) => 0,
            AnnotatedType::SelfType { type_arity, .. } => *type_arity,
            AnnotatedType::TypeVar(AnnotatedTypeVar { type_arity, .. }) => *type_arity,
            AnnotatedType::ConstrainedTypeVar {
                base: AnnotatedTypeVar { type_arity, .. },
                ..
            } => *type_arity,
            AnnotatedType::Unit => 0,
            AnnotatedType::Lambda { .. } => 0,
            AnnotatedType::Tuple(_) => 0,
            AnnotatedType::Bounded { args, .. } => args.len(),
            AnnotatedType::Unconstrained => 0,
            AnnotatedType::Missing => 0,
        }
    }
}

impl std::fmt::Display for AnnotatedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnnotatedType::Unit => write!(f, "()"),
            AnnotatedType::BuiltIn(builtin) => write!(f, "{builtin:?}"),
            AnnotatedType::TypeDef { name, .. } => write!(f, "{name}"),
            // TODO: display type args on bounded type defs
            // AnnotatedType::TypeDef {
            //     name, type_args, ..
            // } => {
            //     write!(f, "{name}")?;
            //
            //     if !type_args.is_empty() {
            //         write!(f, "[")?;
            //         type_args
            //             .iter()
            //             .map(|type_arg| type_arg.name.clone())
            //             .join(", ")
            //             .fmt(f)?;
            //         write!(f, "]")?;
            //     }
            //     Ok(())
            // }
            AnnotatedType::Lambda { arg, ret } => match arg.as_ref() {
                AnnotatedType::Lambda { .. } => write!(f, "({arg}) -> {ret}"),
                _ => write!(f, "{arg} -> {ret}"),
            },
            AnnotatedType::Tuple(elements) => {
                write!(f, "(")?;
                elements.iter().join(", ").fmt(f)?;
                write!(f, ")")
            }
            AnnotatedType::Bounded { base, args } => {
                write!(f, "{base}[")?;
                args.iter().join(", ").fmt(f)?;
                write!(f, "]")
            }
            AnnotatedType::TypeVar(AnnotatedTypeVar { name, .. }) => write!(f, "{name}"),
            AnnotatedType::ConstrainedTypeVar {
                base: AnnotatedTypeVar { name, .. },
                constraints,
                ..
            } => {
                write!(f, "{name} : ")?;
                constraints
                    .iter()
                    .map(|c| &c.trait_name)
                    .join(" + ")
                    .fmt(f)?;
                Ok(())
            }
            AnnotatedType::SelfType {
                trait_constraints: constraints,
                ..
            } => {
                write!(f, "Self")?;
                if !constraints.is_empty() {
                    write!(f, " : ")?;
                    constraints
                        .iter()
                        .map(|c| &c.trait_name)
                        .join(" + ")
                        .fmt(f)?;
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
                let trait_constraints: Vec<TraitConstraint> = trait_def
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

            let expected_arity = base_resolved.type_arity();
            let corrected_args = correct_arity(args_resolved, expected_arity);

            if corrected_args.is_empty() {
                base_resolved
            } else {
                AnnotatedType::Bounded {
                    base: Box::new(base_resolved),
                    args: corrected_args,
                }
            }
        }
    }
}

fn correct_arity(args: Vec<AnnotatedType>, expected_arity: usize) -> Vec<AnnotatedType> {
    if args.len() > expected_arity {
        args[..expected_arity].to_vec()
    } else if args.len() < expected_arity {
        let mut padded = args;
        padded.resize(expected_arity, AnnotatedType::Unconstrained);
        padded
    } else {
        args
    }
}

fn trait_constraints(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    c: &hir::TypeVariableConstraint,
) -> Option<TraitConstraint> {
    match c {
        hir::TypeVariableConstraint::Trait(type_idx) => {
            let trait_fql = crate::resolve_trait_by_ref_id(db, module_id, *type_idx).ok()?;
            let trait_name = trait_fql.trait_name(db);
            Some(TraitConstraint {
                trait_fql,
                trait_name,
            })
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
    let type_var = AnnotatedTypeVar {
        fql,
        name: name.clone(),
        type_arity: 0,
    };

    match kind {
        hir::TypeVariableKind::Unbound => AnnotatedType::TypeVar(type_var),
        hir::TypeVariableKind::Constrained(constraints) => {
            let trait_constraints: Vec<TraitConstraint> = constraints
                .iter()
                .filter_map(|constraint| trait_constraints(db, module_id, constraint))
                .collect();

            let type_arity = {
                if let Some(arity) = constraints.iter().find_map(kind_constraints) {
                    arity
                } else {
                    trait_constraints
                        .iter()
                        .filter_map(|c| {
                            let (trait_module, _) = hir::lower_file(db, c.trait_fql.module_id);
                            let trait_def = trait_module.get_trait(c.trait_fql.local_id);
                            trait_def
                                .self_constraints()
                                .iter()
                                .find_map(kind_constraints)
                        })
                        .next()
                        .unwrap_or(0)
                }
            };

            let type_var = AnnotatedTypeVar {
                type_arity,
                ..type_var
            };

            NonEmpty::try_from(trait_constraints)
                .map(|constraints| AnnotatedType::ConstrainedTypeVar {
                    base: type_var.clone(),
                    constraints,
                })
                .unwrap_or_else(|_| AnnotatedType::TypeVar(type_var))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestHirResDatabase;
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

        let actual_type = resolve_annotated_type(&db, module_id, idx!(0));

        db.attach(|_| {
            assert_eq!(AnnotatedType::BuiltIn(hir::BuiltInType::Int), actual_type);
        });
    }

    #[test]
    fn resolve_cross_module_lambda_with_no_type_annotation_uses_body() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let test_data_module_id = db.add_test_module(
            "test_data",
            r"
            typedef Test[t] = Thing t
            let test = Test(0)
            let new = |t| -> Test(t)
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

        let actual_type = resolve_annotated_type(&db, module_id, idx!(13));

        let type_var_t1 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(0)),
            name: hir::Name::from("t1"),
            type_arity: 0,
        });
        let type_var_t2 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(1)),
            name: hir::Name::from("t2"),
            type_arity: 0,
        });

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2.clone()),
                        ret: Box::new(type_var_t1.clone()),
                    }),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2.clone()),
                        ret: Box::new(AnnotatedType::Lambda {
                            arg: Box::new(type_var_t2.clone()),
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
                                        type_var_t1.clone(),
                                        type_var_t1.clone(),
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
    fn resolve_cross_module_value_with_no_type_annotation_uses_body() {
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

        let actual_type = resolve_annotated_type(&db, module_id, idx!(6));

        let type_var_t1 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(0)),
            name: hir::Name::from("t1"),
            type_arity: 0,
        });
        let type_var_t2 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(1)),
            name: hir::Name::from("t2"),
            type_arity: 0,
        });

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(type_var_t1),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2),
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
                            args: vec![AnnotatedType::BuiltIn(hir::BuiltInType::Int)],
                        }),
                    }),
                },
                actual_type,
            );
        });
    }

    #[test]
    fn resolve_unbound_typedef_reference_when_typedef_has_type_args() {
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

            typeof f : t1 -> t2 -> test_data::Test where
              typevar t1
              typevar t2
            let f = |a, b| -> test
            ",
        );

        let actual_type = resolve_annotated_type(&db, module_id, idx!(4));

        let type_var_t1 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(0)),
            name: hir::Name::from("t1"),
            type_arity: 0,
        });
        let type_var_t2 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(1)),
            name: hir::Name::from("t2"),
            type_arity: 0,
        });

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(type_var_t1),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2),
                        ret: Box::new(AnnotatedType::TypeDef {
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
                    }),
                },
                actual_type,
            );
        });
    }

    #[test]
    fn resolve_bounded_typedef_reference_with_too_many_args_removes_extras() {
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

            typeof f : t1 -> t2 -> test_data::Test[Int, Int] where
              typevar t1
              typevar t2
            let f = |a, b| -> test
            ",
        );

        let actual_type = resolve_annotated_type(&db, module_id, idx!(7));

        let type_var_t1 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(0)),
            name: hir::Name::from("t1"),
            type_arity: 0,
        });
        let type_var_t2 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(1)),
            name: hir::Name::from("t2"),
            type_arity: 0,
        });

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(type_var_t1),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2),
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
                            args: vec![AnnotatedType::BuiltIn(hir::BuiltInType::Int)],
                        }),
                    }),
                },
                actual_type,
            );
        });
    }

    #[test]
    fn resolve_bounded_typedef_reference_with_too_few_args_pads_with_unbounded() {
        let mut db = TestHirResDatabase::new_with_stdlib();
        let test_data_module_id = db.add_module(
            "test_data",
            camino::Utf8Path::new("./test/test_data.alloy"),
            r"
            typedef Pair[t1, t2] = Pair t1 t2
            let test = Pair(0, 0)
            ",
        );
        let module_id = db.add_module(
            "test",
            camino::Utf8Path::new("./test.alloy"),
            r"
            import test_data::test
            import test_data

            typeof f : t1 -> t2 -> test_data::Pair[Int] where
              typevar t1
              typevar t2
            let f = |a, b| -> test
            ",
        );

        let actual_type = resolve_annotated_type(&db, module_id, idx!(6));

        let type_var_t1 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(0)),
            name: hir::Name::from("t1"),
            type_arity: 0,
        });
        let type_var_t2 = AnnotatedType::TypeVar(AnnotatedTypeVar {
            fql: Fql::new(module_id, idx!(1)),
            name: hir::Name::from("t2"),
            type_arity: 0,
        });

        db.attach(|_| {
            assert_eq!(
                AnnotatedType::Lambda {
                    arg: Box::new(type_var_t1),
                    ret: Box::new(AnnotatedType::Lambda {
                        arg: Box::new(type_var_t2),
                        ret: Box::new(AnnotatedType::Bounded {
                            base: Box::new(AnnotatedType::TypeDef {
                                fql: Fql::new(test_data_module_id, idx!(0)),
                                name: hir::Name::from("Pair"),
                                type_args: vec![
                                    TypeVarReference {
                                        fql: Fql {
                                            module_id: test_data_module_id,
                                            local_id: idx!(0),
                                        },
                                        name: hir::Name::from("t1"),
                                    },
                                    TypeVarReference {
                                        fql: Fql {
                                            module_id: test_data_module_id,
                                            local_id: idx!(1),
                                        },
                                        name: hir::Name::from("t2"),
                                    }
                                ],
                            }),
                            args: vec![
                                AnnotatedType::BuiltIn(hir::BuiltInType::Int),
                                AnnotatedType::Unconstrained
                            ],
                        }),
                    }),
                },
                actual_type,
            );
        });
    }
}
