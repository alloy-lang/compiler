//! Type annotation checking and compatibility validation
//!
//! This module handles checking that inferred types are compatible with their
//! type annotations, including trait constraint verification.

use crate::diagnostics::{ConflictingTypeAnnotationReason, TypeCheckingErrorKind};
use crate::{HirTyDatabase, HirTypedModule};
use alloy_hir_def as hir;
use alloy_hir_def::{ExpressionIdx, PatternIdx, TypeIdx};
use alloy_hir_infer::InferredType;
use alloy_hir_resolved as res;
use alloy_hir_resolved::{resolve_annotated_type, AnnotatedType, Fql};
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Clone, Copy)]
enum InferredSource {
    Expression(ExpressionIdx),
    Pattern(PatternIdx),
    /// Inside a curried lambda — the HIR has a flat `Lambda { args, body }` but
    /// the inferred type is curried (`A -> B -> C`). This variant walks through
    /// args one at a time as the checker recurses through nested `Lambda` types.
    /// Range resolution returns the whole lambda expression's range, since the
    /// curried intermediate doesn't have its own distinct source span.
    CurriedLambda {
        expr_idx: ExpressionIdx,
        arg_offset: usize,
    },
}

pub(crate) fn validate_type_annotations(
    db: &dyn HirTyDatabase,
    module_id: ModuleId,
    result: &mut HirTypedModule,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    for value_def in hir_module.values().map(|(_, v)| v) {
        let idx = value_def.expr_idx;
        let Some(resolved_type) = result.expression_types.get(&idx).cloned() else {
            continue;
        };
        let range = hir_module.get_expression_range(idx);

        if let Some(type_annotation) = value_def.type_annotation {
            // Validate arity of bounded types in the annotation
            validate_type_reference_arity(db, result, module_id, type_annotation);
            // Check for type annotation conflicts
            check_type_annotation(
                db,
                result,
                module_id,
                range,
                type_annotation,
                resolved_type,
                idx,
            );
        }
    }
}

fn check_type_annotation(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    current_module_id: ModuleId,
    range: TextRange,
    type_annotation_idx: TypeIdx,
    resolved_type: InferredType,
    expr_idx: ExpressionIdx,
) {
    let expected_type = resolve_annotated_type(db, current_module_id, type_annotation_idx);
    let (hir_module, _) = hir::lower_file(db, current_module_id);
    let annotation_range = hir_module.get_type_reference_range(type_annotation_idx);

    // Skip check for missing/unconstrained annotations
    if matches!(
        expected_type,
        AnnotatedType::Missing | AnnotatedType::Unconstrained
    ) {
        return;
    }

    // Check if the inferred type is compatible with the expected type
    let mut checker = TypeAnnotationChecker::new(db, current_module_id);
    if let Err(reason) = checker.check_type_compatibility(
        &expected_type,
        &resolved_type,
        type_annotation_idx,
        InferredSource::Expression(expr_idx),
    ) {
        result.error(
            TypeCheckingErrorKind::ConflictingTypeAnnotation {
                annotated_type: expected_type,
                annotation_range,
                value_type: resolved_type,
                value_range: range,
                reason,
            },
            range,
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AnnotationVarId {
    TypeVar(Fql<hir::TypeVariable>),
    SelfType(Fql<hir::Trait>),
}

type GenericMapping = FxHashMap<usize, AnnotationVarId>;

struct TypeAnnotationChecker<'db> {
    db: &'db dyn HirTyDatabase,
    hir_module: hir::HirModule,
    generic_mapping: GenericMapping,
}

impl<'db> TypeAnnotationChecker<'db> {
    fn new(db: &'db dyn HirTyDatabase, module_id: ModuleId) -> Self {
        let (hir_module, _) = hir::lower_file(db, module_id);
        Self {
            db,
            hir_module,
            generic_mapping: GenericMapping::default(),
        }
    }

    /// Look up the source range for a type annotation index.
    fn annotation_range(&self, type_idx: TypeIdx) -> TextRange {
        self.hir_module.get_type_reference_range(type_idx)
    }

    /// Look up the source range for an inferred type source.
    fn inferred_range(&self, source: InferredSource) -> TextRange {
        match source {
            InferredSource::Expression(idx) => self.hir_module.get_expression_range(idx),
            InferredSource::Pattern(idx) => self.hir_module.get_pattern_range(idx),
            InferredSource::CurriedLambda { expr_idx, .. } => {
                self.hir_module.get_expression_range(expr_idx)
            }
        }
    }

    fn direct_conflict(
        &self,
        expected: &AnnotatedType,
        found: &InferredType,
        type_idx: TypeIdx,
        source: InferredSource,
    ) -> ConflictingTypeAnnotationReason {
        ConflictingTypeAnnotationReason::DirectConflict {
            expected_type: expected.clone(),
            expected_type_range: self.annotation_range(type_idx),
            actual_type: found.clone(),
            actual_type_range: self.inferred_range(source),
        }
    }

    /// Decompose an `InferredSource` for a Lambda type's arg and return positions.
    ///
    /// When the source points at a lambda expression (`|x, y| -> body`), the
    /// curried type `A -> B -> C` maps to:
    ///   - arg: the pattern for `args[offset]`
    ///   - ret: either the next curried arg or the body expression
    fn lambda_sources(&self, source: InferredSource) -> (InferredSource, InferredSource) {
        let (expr_idx, offset) = match source {
            InferredSource::Expression(idx) => (idx, 0),
            InferredSource::CurriedLambda {
                expr_idx,
                arg_offset,
            } => (expr_idx, arg_offset),
            // Pattern source doesn't decompose into lambda parts
            InferredSource::Pattern(_) => return (source, source),
        };

        let expr = self.hir_module.get_expression(expr_idx);
        match expr {
            hir::Expression::Lambda { args, body } if offset < args.len() => {
                let arg_source = InferredSource::Pattern(args[offset]);
                let ret_source = if offset + 1 < args.len() {
                    InferredSource::CurriedLambda {
                        expr_idx,
                        arg_offset: offset + 1,
                    }
                } else {
                    InferredSource::Expression(*body)
                };
                (arg_source, ret_source)
            }
            _ => (source, source),
        }
    }

    /// Check if the `found` type is compatible with the `expected` annotation type.
    /// `type_idx` is the HIR type reference index for the annotation, used to look up source ranges.
    /// `inferred_source` tracks the position in the inferred type tree for error ranges.
    fn check_type_compatibility(
        &mut self,
        expected: &AnnotatedType,
        found: &InferredType,
        expected_type_idx: TypeIdx,
        inferred_source: InferredSource,
    ) -> Result<(), ConflictingTypeAnnotationReason> {
        match (expected, found) {
            // Wildcards on either side
            (AnnotatedType::Unconstrained, _) | (_, InferredType::Unconstrained) => Ok(()),
            (AnnotatedType::Missing, _) | (_, InferredType::Missing) => {
                Err(self.direct_conflict(expected, found, expected_type_idx, inferred_source))
            }

            // Unit
            (AnnotatedType::Unit, InferredType::Unit) => Ok(()),

            // Built-in types
            (AnnotatedType::BuiltIn(a), InferredType::BuiltIn(b)) if a == b => Ok(()),

            // Nominal type definitions
            (AnnotatedType::TypeDef { fql: a, .. }, InferredType::TypeDef(b, _)) if a == b => {
                Ok(())
            }

            // Type variables in annotation match any generic in inference result
            (AnnotatedType::TypeVar(atv), InferredType::Generic(id, _)) => self
                .check_generic_consistency(
                    AnnotationVarId::TypeVar(atv.fql.clone()),
                    *id,
                    expected,
                    found,
                    expected_type_idx,
                    inferred_source,
                ),
            // Unconstrained annotation type var vs constrained inferred generic — insufficient
            (
                AnnotatedType::TypeVar(atv),
                InferredType::ConstrainedGeneric {
                    id, constraints, ..
                },
            ) => {
                self.check_generic_consistency(
                    AnnotationVarId::TypeVar(atv.fql.clone()),
                    *id,
                    expected,
                    found,
                    expected_type_idx,
                    inferred_source,
                )?;
                check_constraint_sufficiency(self.db, &[], constraints)
            }

            // Constrained type variables - check consistency + trait constraints
            (AnnotatedType::ConstrainedTypeVar { base, .. }, InferredType::Generic(id, _)) => self
                .check_generic_consistency(
                    AnnotationVarId::TypeVar(base.fql.clone()),
                    *id,
                    expected,
                    found,
                    expected_type_idx,
                    inferred_source,
                ),
            (
                AnnotatedType::ConstrainedTypeVar {
                    base,
                    constraints: annotation_constraints,
                },
                InferredType::ConstrainedGeneric {
                    id,
                    constraints: inferred_constraints,
                    ..
                },
            ) => {
                self.check_generic_consistency(
                    AnnotationVarId::TypeVar(base.fql.clone()),
                    *id,
                    expected,
                    found,
                    expected_type_idx,
                    inferred_source,
                )?;
                check_constraint_sufficiency(self.db, annotation_constraints, inferred_constraints)
            }
            (AnnotatedType::ConstrainedTypeVar { .. }, _found_ty) => Ok(()), // constraints checked during unification

            // Self type in annotation matches generics
            (AnnotatedType::SelfType { trait_fql, .. }, InferredType::Generic(id, _)) => self
                .check_generic_consistency(
                    AnnotationVarId::SelfType(trait_fql.clone()),
                    *id,
                    expected,
                    found,
                    expected_type_idx,
                    inferred_source,
                ),
            (
                AnnotatedType::SelfType { trait_fql, .. },
                InferredType::ConstrainedGeneric { id, .. },
            ) => self.check_generic_consistency(
                AnnotationVarId::SelfType(trait_fql.clone()),
                *id,
                expected,
                found,
                expected_type_idx,
                inferred_source,
            ),
            (
                AnnotatedType::SelfType {
                    trait_constraints: constraints,
                    ..
                },
                _found_ty,
            ) if !constraints.is_empty() => Ok(()), // constraints checked during unification

            // Lambda types
            (
                AnnotatedType::Lambda { arg, ret },
                InferredType::Lambda {
                    arg_type,
                    return_type,
                },
            ) => {
                let (arg_idx, ret_idx) = match self.hir_module.get_type_reference(expected_type_idx)
                {
                    hir::TypeReference::Lambda {
                        arg_type,
                        return_type,
                        ..
                    } => (*arg_type, *return_type),
                    _ => (expected_type_idx, expected_type_idx),
                };
                let (arg_source, ret_source) = self.lambda_sources(inferred_source);
                self.check_type_compatibility(arg, arg_type, arg_idx, arg_source)?;
                self.check_type_compatibility(ret, return_type, ret_idx, ret_source)?;
                Ok(())
            }

            // Tuple types
            (AnnotatedType::Tuple(exp_elems), InferredType::Tuple(found_elems)) => {
                if exp_elems.len() != found_elems.len() {
                    return Err(self.direct_conflict(
                        expected,
                        found,
                        expected_type_idx,
                        inferred_source,
                    ));
                }
                let elem_indices = match self.hir_module.get_type_reference(expected_type_idx) {
                    hir::TypeReference::Tuple(indices) => Some(indices.clone()),
                    _ => None,
                };
                for (i, (exp_elem, found_elem)) in
                    exp_elems.iter().zip(found_elems.iter()).enumerate()
                {
                    let elem_idx = elem_indices
                        .as_ref()
                        .and_then(|indices| indices.get(i).copied())
                        .unwrap_or(expected_type_idx);
                    self.check_type_compatibility(exp_elem, found_elem, elem_idx, inferred_source)?;
                }
                Ok(())
            }

            (
                AnnotatedType::Bounded { base, .. },
                InferredType::Generic(..) | InferredType::ConstrainedGeneric { .. },
            ) if matches!(
                base.as_ref(),
                AnnotatedType::TypeVar(_)
                    | AnnotatedType::ConstrainedTypeVar { .. }
                    | AnnotatedType::SelfType { .. }
            ) =>
            {
                Ok(())
            }

            // Bounded types
            (
                AnnotatedType::Bounded {
                    base: exp_base,
                    args: exp_args,
                },
                InferredType::Bounded {
                    base: found_base,
                    args: found_args,
                },
            ) => {
                if exp_args.len() != found_args.len() {
                    return Err(self.direct_conflict(
                        expected,
                        found,
                        expected_type_idx,
                        inferred_source,
                    ));
                }
                let (base_idx, arg_indices) =
                    match self.hir_module.get_type_reference(expected_type_idx) {
                        hir::TypeReference::Bounded { base, args } => (*base, Some(args.clone())),
                        _ => (expected_type_idx, None),
                    };
                self.check_type_compatibility(exp_base, found_base, base_idx, inferred_source)?;
                for (i, (exp_arg, found_arg)) in exp_args.iter().zip(found_args.iter()).enumerate()
                {
                    let arg_idx = arg_indices
                        .as_ref()
                        .and_then(|indices| indices.get(i).copied())
                        .unwrap_or(expected_type_idx);
                    self.check_type_compatibility(exp_arg, found_arg, arg_idx, inferred_source)?;
                }
                Ok(())
            }

            // Everything else is incompatible
            _ => Err(self.direct_conflict(expected, found, expected_type_idx, inferred_source)),
        }
    }

    /// Check that different annotation type variables don't claim the same inferred generic ID.
    fn check_generic_consistency(
        &mut self,
        var_id: AnnotationVarId,
        generic_id: usize,
        expected: &AnnotatedType,
        found: &InferredType,
        type_idx: TypeIdx,
        inferred_source: InferredSource,
    ) -> Result<(), ConflictingTypeAnnotationReason> {
        if let Some(prev_var) = self.generic_mapping.get(&generic_id) {
            if *prev_var != var_id {
                return Err(self.direct_conflict(expected, found, type_idx, inferred_source));
            }
        } else {
            self.generic_mapping.insert(generic_id, var_id);
        }
        Ok(())
    }
}

/// Check that the annotation declares all constraints the body requires.
fn check_constraint_sufficiency(
    db: &dyn HirTyDatabase,
    annotation_constraints: &[res::TraitConstraint],
    inferred_constraints: &NonEmpty<res::TraitConstraint>,
) -> Result<(), ConflictingTypeAnnotationReason> {
    let mut satisfied: Vec<Fql<hir::Trait>> = Vec::new();
    for ac in annotation_constraints {
        if !satisfied.contains(&ac.trait_fql) {
            satisfied.push(ac.trait_fql.clone());
        }
    }

    let missing: Vec<_> = inferred_constraints
        .iter()
        .filter(|ic| !satisfied.contains(&ic.trait_fql))
        .cloned()
        .map(|ic| {
            let (hir_module, _) = hir::lower_file(db, ic.type_var_constraint_fql.module_id);
            let range =
                hir_module.get_type_variable_constraint_range(ic.type_var_constraint_fql.local_id);
            (ic, range)
        })
        .collect();

    if missing.is_empty() {
        Ok(())
    } else {
        Err(ConflictingTypeAnnotationReason::InsufficientConstraints {
            missing_constraints: missing,
        })
    }
}

/// Walk a type reference tree and report arity errors at each Bounded node.
fn validate_type_reference_arity(
    db: &dyn HirTyDatabase,
    result: &mut HirTypedModule,
    module_id: ModuleId,
    type_idx: TypeIdx,
) {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Bounded { base, args } => {
            let base_annotated = resolve_annotated_type(db, module_id, *base);
            let expected_arity = base_annotated.type_arity();

            if expected_arity != args.len() {
                let annotation_range = hir_module.get_type_reference_range(type_idx);
                let type_name = hir::Name::new(format!("{}", base_annotated));
                result.error(
                    TypeCheckingErrorKind::BoundedTypeArityMismatch {
                        type_name,
                        expected_arity,
                        actual_arity: args.len(),
                        annotation_range,
                    },
                    annotation_range,
                );
            }

            // Recurse into args only — base is expected to have arity
            for arg in args {
                validate_type_reference_arity(db, result, module_id, *arg);
            }
        }
        hir::TypeReference::Lambda {
            arg_type,
            return_type,
        } => {
            validate_type_reference_arity(db, result, module_id, *arg_type);
            validate_type_reference_arity(db, result, module_id, *return_type);
        }
        hir::TypeReference::Tuple(types) => {
            for t in types {
                validate_type_reference_arity(db, result, module_id, *t);
            }
        }
        hir::TypeReference::ParenthesizedType(inner) => {
            validate_type_reference_arity(db, result, module_id, *inner);
        }
        // Bare type reference — check if it expects type args
        hir::TypeReference::Named(_) => {
            let resolved = resolve_annotated_type(db, module_id, type_idx);
            let expected_arity = resolved.type_arity();
            if expected_arity > 0 {
                let annotation_range = hir_module.get_type_reference_range(type_idx);
                let type_name = hir::Name::new(format!("{}", resolved));
                result.error(
                    TypeCheckingErrorKind::BoundedTypeArityMismatch {
                        type_name,
                        expected_arity,
                        actual_arity: 0,
                        annotation_range,
                    },
                    annotation_range,
                );
            }
        }
        // Leaf nodes — nothing to check
        hir::TypeReference::Unconstrained
        | hir::TypeReference::Missing
        | hir::TypeReference::SelfRef(_)
        | hir::TypeReference::Unit
        | hir::TypeReference::BuiltIn(_) => {}
    }
}
