use crate::hir_ty::hm::{MonoType, TypeVarId};
use crate::{DisplayName, InferredType};
use alloy_hir_def as hir;
use alloy_hir_resolved::{AnnotatedType, AnnotatedTypeVar, Fql};
use non_empty_vec::NonEmpty;
use rustc_hash::{FxHashMap, FxHashSet};

pub(super) struct TypeVarGenerator {
    next_id: usize,
}

impl TypeVarGenerator {
    fn new() -> Self {
        Self { next_id: 0 }
    }

    pub(super) fn fresh(&mut self) -> TypeVarId {
        let id = self.next_id;
        self.next_id += 1;
        TypeVarId::new(id)
    }
}

pub(super) struct ToInferredTypeConverter {
    pub(super) type_var_gen: TypeVarGenerator,

    type_var_names: FxHashMap<TypeVarId, hir::Name>,
    seen: FxHashMap<TypeVarId, usize>,
}

impl ToInferredTypeConverter {
    pub(super) fn from(other: &ToMonoTypeConverter) -> Self {
        Self {
            type_var_gen: TypeVarGenerator::new(),
            type_var_names: other.type_var_names.clone(),
            seen: FxHashMap::default(),
        }
    }

    pub(super) fn mono_to_inferred(
        &mut self,
        mono: &MonoType,
        failed_vars: &FxHashSet<TypeVarId>,
    ) -> InferredType {
        match mono {
            MonoType::Unconstrained => InferredType::Unconstrained,
            MonoType::Var(var_id) => {
                if failed_vars.contains(var_id) {
                    return InferredType::Missing;
                }
                let (generic_id, name) = self.resolve_generic(*var_id);
                InferredType::Generic(generic_id, name)
            }
            MonoType::ConstrainedVar(var_id, constraints) => {
                if failed_vars.contains(var_id) {
                    return InferredType::Missing;
                }
                let (generic_id, name) = self.resolve_generic(*var_id);
                if let Some((first, rest)) = constraints.split_first() {
                    InferredType::ConstrainedGeneric {
                        id: generic_id,
                        name,
                        constraints: NonEmpty::from((first.clone(), rest.to_vec())),
                    }
                } else {
                    InferredType::Generic(generic_id, name)
                }
            }
            MonoType::Concrete(builtin) => InferredType::BuiltIn(*builtin),
            MonoType::Function(arg, ret) => InferredType::Lambda {
                arg_type: Box::new(self.mono_to_inferred(arg, failed_vars)),
                return_type: Box::new(self.mono_to_inferred(ret, failed_vars)),
            },
            MonoType::Tuple(elements) => {
                let resolved_elements: Vec<_> = elements
                    .iter()
                    .map(|e| self.mono_to_inferred(e, failed_vars))
                    .collect();
                if resolved_elements.is_empty() {
                    InferredType::Unit
                } else {
                    let first = resolved_elements[0].clone();
                    let rest = resolved_elements.into_iter().skip(1).collect();
                    InferredType::Tuple(NonEmpty::from((first, rest)))
                }
            }
            MonoType::TypeDef {
                fql, type_def_name, ..
            } => InferredType::TypeDef(fql.clone(), type_def_name.clone()),
            MonoType::App { constructor, args } => {
                let base = self.mono_to_inferred(constructor, failed_vars);
                let resolved_args: Vec<_> = args
                    .iter()
                    .map(|a| self.mono_to_inferred(a, failed_vars))
                    .collect();
                InferredType::Bounded {
                    base: Box::new(base),
                    args: resolved_args,
                }
            }
            MonoType::Unit => InferredType::Unit,
        }
    }

    fn resolve_generic(&mut self, var_id: TypeVarId) -> (usize, DisplayName) {
        let generic_id = if let Some(generic_id) = self.seen.get(&var_id) {
            *generic_id
        } else {
            let new_id = self.type_var_gen.fresh().0;
            self.seen.insert(var_id, new_id);
            new_id
        };

        let name = self
            .type_var_names
            .get(&var_id)
            .map(|n| n.to_string())
            .unwrap_or_else(|| {
                let letter = (b'a' + (generic_id % 26) as u8) as char;
                let suffix = generic_id / 26;
                format!("{letter}{suffix}")
            });
        (generic_id, DisplayName::new(name))
    }
}

pub(super) struct ToMonoTypeConverter {
    pub(super) type_var_gen: TypeVarGenerator,

    type_var_names: FxHashMap<TypeVarId, hir::Name>,
    annotation_type_vars: FxHashMap<Fql<hir::TypeVariable>, TypeVarId>,
    self_type_vars: FxHashMap<Fql<hir::Trait>, TypeVarId>,
}

impl ToMonoTypeConverter {
    pub(super) fn empty() -> Self {
        Self {
            type_var_gen: TypeVarGenerator::new(),
            type_var_names: FxHashMap::default(),
            annotation_type_vars: FxHashMap::default(),
            self_type_vars: FxHashMap::default(),
        }
    }

    pub(super) fn annotated_to_mono(&mut self, annotated: &AnnotatedType) -> Option<MonoType> {
        match annotated {
            AnnotatedType::Missing => None,
            AnnotatedType::Unconstrained => Some(MonoType::Unconstrained),
            AnnotatedType::Unit => Some(MonoType::Unit),
            AnnotatedType::BuiltIn(builtin) => Some(MonoType::Concrete(*builtin)),
            AnnotatedType::TypeDef {
                fql,
                name,
                type_args,
            } => {
                let type_args = type_args
                    .iter()
                    .map(|ty_arg| {
                        self.get_or_create_annotation_type_var(
                            ty_arg.fql.clone(),
                            ty_arg.name.clone(),
                        )
                    })
                    .collect::<Vec<_>>();
                Some(MonoType::TypeDef {
                    fql: fql.clone(),
                    type_args,
                    type_def_name: name.clone(),
                })
            }
            AnnotatedType::Lambda { arg, ret } => {
                let arg_mono = self.annotated_to_mono(arg)?;
                let ret_mono = self.annotated_to_mono(ret)?;
                Some(MonoType::Function(Box::new(arg_mono), Box::new(ret_mono)))
            }
            AnnotatedType::Tuple(elements) => {
                let mono_elements: Option<Vec<_>> =
                    elements.iter().map(|e| self.annotated_to_mono(e)).collect();
                mono_elements.map(MonoType::Tuple)
            }
            AnnotatedType::Bounded { base, args } => {
                let base_mono = self.annotated_to_mono(base)?;
                let args_mono: Option<Vec<_>> =
                    args.iter().map(|a| self.annotated_to_mono(a)).collect();
                Some(MonoType::App {
                    constructor: Box::new(base_mono),
                    args: args_mono?,
                })
            }
            AnnotatedType::TypeVar(AnnotatedTypeVar { name, fql, .. }) => {
                let var_id = self.get_or_create_annotation_type_var(fql.clone(), name.clone());
                Some(MonoType::Var(var_id))
            }
            AnnotatedType::ConstrainedTypeVar {
                base: AnnotatedTypeVar { name, fql, .. },
                constraints,
            } => {
                let var_id = self.get_or_create_annotation_type_var(fql.clone(), name.clone());
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
                let var_id = self.get_or_create_self_type_var(trait_fql.clone());
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

    /// Convert an InferredType (from `infer_value_signature`) to a MonoType for use
    /// in constraint generation. Generic IDs are mapped to fresh type variables,
    /// with consistent mapping so the same Generic(id) produces the same TypeVarId.
    pub(super) fn inferred_to_mono(&mut self, inferred: &InferredType) -> MonoType {
        let mut generic_map: FxHashMap<usize, TypeVarId> = FxHashMap::default();
        self.inferred_to_mono_inner(inferred, &mut generic_map)
    }

    fn inferred_to_mono_inner(
        &mut self,
        inferred: &InferredType,
        generic_map: &mut FxHashMap<usize, TypeVarId>,
    ) -> MonoType {
        match inferred {
            InferredType::Unconstrained => MonoType::Unconstrained,
            InferredType::Missing => MonoType::Var(self.fresh_type_var()),
            InferredType::Unit => MonoType::Unit,
            InferredType::BuiltIn(b) => MonoType::Concrete(*b),
            InferredType::TypeDef(fql, name) => MonoType::TypeDef {
                fql: fql.clone(),
                type_args: vec![],
                type_def_name: name.clone(),
            },
            InferredType::Lambda {
                arg_type,
                return_type,
            } => MonoType::Function(
                Box::new(self.inferred_to_mono_inner(arg_type, generic_map)),
                Box::new(self.inferred_to_mono_inner(return_type, generic_map)),
            ),
            InferredType::Tuple(elements) => MonoType::Tuple(
                elements
                    .iter()
                    .map(|e| self.inferred_to_mono_inner(e, generic_map))
                    .collect(),
            ),
            InferredType::Bounded { base, args } => {
                // When the base is a TypeDef, populate its type_args with fresh TypeVarIds
                // to preserve arity information (used in error messages and display)
                let constructor = match base.as_ref() {
                    InferredType::TypeDef(fql, name) => {
                        let type_args = args.iter().map(|_| self.fresh_type_var()).collect();
                        MonoType::TypeDef {
                            fql: fql.clone(),
                            type_args,
                            type_def_name: name.clone(),
                        }
                    }
                    other => self.inferred_to_mono_inner(other, generic_map),
                };
                MonoType::App {
                    constructor: Box::new(constructor),
                    args: args
                        .iter()
                        .map(|a| self.inferred_to_mono_inner(a, generic_map))
                        .collect(),
                }
            }
            InferredType::Generic(id, name) => {
                let var_id = *generic_map
                    .entry(*id)
                    .or_insert_with(|| self.fresh_type_var());
                self.type_var_names.insert(var_id, hir::Name::new(&name.0));
                MonoType::Var(var_id)
            }
            InferredType::ConstrainedGeneric {
                id,
                constraints,
                name,
            } => {
                let var_id = *generic_map
                    .entry(*id)
                    .or_insert_with(|| self.fresh_type_var());
                self.type_var_names.insert(var_id, hir::Name::new(&name.0));
                MonoType::ConstrainedVar(var_id, constraints.iter().cloned().collect())
            }
        }
    }

    pub(super) fn get_or_create_annotation_type_var(
        &mut self,
        fql: Fql<hir::TypeVariable>,
        name: hir::Name,
    ) -> TypeVarId {
        if let Some(&var_id) = self.annotation_type_vars.get(&fql) {
            return var_id;
        }
        let var_id = self.fresh_type_var();
        self.annotation_type_vars.insert(fql, var_id);
        self.type_var_names.insert(var_id, name);
        var_id
    }

    fn get_or_create_self_type_var(&mut self, trait_fql: Fql<hir::Trait>) -> TypeVarId {
        if let Some(&var_id) = self.self_type_vars.get(&trait_fql) {
            return var_id;
        }
        let var_id = self.fresh_type_var();
        self.self_type_vars.insert(trait_fql, var_id);
        self.type_var_names.insert(var_id, hir::Name::new("Self"));
        var_id
    }

    pub(super) fn fresh_type_var(&mut self) -> TypeVarId {
        self.type_var_gen.fresh()
    }
}
