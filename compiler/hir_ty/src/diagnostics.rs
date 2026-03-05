use crate::hir_ty::ResolvedType;
use alloy_diagnostics::{Diagnostic, DiagnosticBuilder, Severity};
use alloy_hir as hir;
use alloy_hir::FqnResolutionError;
use alloy_hir_resolved::{AnnotatedType, HirResolutionError};
use itertools::Itertools;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceError {
    kind: TypeInferenceErrorKind,
    range: TextRange,
}

impl TypeInferenceError {
    #[must_use]
    pub fn new(kind: TypeInferenceErrorKind, range: TextRange) -> Self {
        Self { kind, range }
    }

    /// Get the text range where this error occurred
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// Get the kind of this error
    pub fn kind(&self) -> &TypeInferenceErrorKind {
        &self.kind
    }
}

impl Diagnostic for TypeInferenceError {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn code(&self) -> Option<&str> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { .. } => Some("E001"),
            TypeInferenceErrorKind::UnificationError(_) => Some("E002"),
            TypeInferenceErrorKind::HirResolutionError { .. } => Some("E003"),
            TypeInferenceErrorKind::MissingTraitMemberImplementation { .. } => Some("E004"),
        }
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation {
                annotated_type,
                inferred_type,
                ..
            } => {
                format!("Type annotation conflict: expected `{annotated_type}`, found `{inferred_type}`")
            }
            TypeInferenceErrorKind::UnificationError(unif_err) => match unif_err {
                crate::hir_ty::UnificationError::TypeMismatch(expected, found) => {
                    format!("Type mismatch: expected `{}`, found `{}`", expected, found)
                }
                crate::hir_ty::UnificationError::OccursCheck(var, ty) => {
                    format!("Infinite type detected: `{}` occurs in `{}`", var, ty)
                }
            },
            TypeInferenceErrorKind::HirResolutionError(err) => match err {
                HirResolutionError::UnresolvedModule { err, .. } => match err {
                    FqnResolutionError::UnknownRootModule {
                        attempted_module_path,
                    } => {
                        format!(
                            "Cannot find module '{}'",
                            attempted_module_path
                                .iter()
                                .map(hir::Name::as_str)
                                .collect::<Vec<_>>()
                                .join("::")
                        )
                    }
                    FqnResolutionError::UnknownChildModule {
                        module_id,
                        unknown_child,
                        ..
                    } => {
                        format!(
                            "Module '{}' does not contain child module '{}'",
                            module_id, unknown_child,
                        )
                    }
                    FqnResolutionError::MissingLocalName { module_id } => {
                        format!("Module '{}' found but missing local name", module_id)
                    }
                    FqnResolutionError::ExtraSegments {
                        fqn,
                        extra_segments,
                    } => {
                        format!(
                            "Module '{}' found but extra path segments '{}' were not resolved",
                            fqn.module_id,
                            extra_segments
                                .iter()
                                .map(hir::Name::as_str)
                                .collect::<Vec<_>>()
                                .join("::")
                        )
                    }
                },
                HirResolutionError::UnknownExpressionReference { path, .. } => {
                    let path_str = path
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    format!("Cannot find value `{}` in this scope", path_str)
                }
                HirResolutionError::UnknownPatternReference { path, .. } => {
                    let path_str = path
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    format!("Cannot find pattern `{}` in this scope", path_str)
                }
                HirResolutionError::UnknownTypeReference { path, .. } => {
                    let path_str = path
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    format!("Cannot find type `{}` in this scope", path_str)
                }
                HirResolutionError::UnknownTypeDefinition { path, .. } => {
                    let path_str = path
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    format!("Cannot find type definition `{}`", path_str)
                }
                HirResolutionError::UnknownTypeDefinitionVariant { variant_name, .. } => {
                    format!("Unknown variant `{}`", variant_name.as_str())
                }
                HirResolutionError::MissingTypeDefinitionVariant { .. } => {
                    "Missing variant name for multi-variant type".to_string()
                }
                HirResolutionError::UnknownTraitReference { path, .. } => {
                    let path_str = path
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    format!("Cannot find trait `{}`", path_str)
                }
                HirResolutionError::UnknownTraitMember { subname, .. } => {
                    format!("Cannot find trait member `{}`", subname.as_str())
                }
                HirResolutionError::BoundedTraitReference { .. } => {
                    "Bounded trait reference resolution not yet implemented".to_string()
                }
            },
            TypeInferenceErrorKind::MissingTraitMemberImplementation {
                trait_name,
                member_name,
                ..
            } => {
                format!(
                    "Behavior must implement abstract member `{}` from trait `{}`",
                    member_name, trait_name
                )
            }
        }
    }

    fn primary_span(&self) -> TextRange {
        self.range
    }

    fn build_report<'a>(&self, builder: DiagnosticBuilder<'a>) -> DiagnosticBuilder<'a> {
        match &self.kind {
            TypeInferenceErrorKind::ConflictingTypeAnnotation { reason, .. } => {
                match reason {
                    ConflictingTypeAnnotationReason::DirectConflict { annotated_type: inner_ea, inferred_type: inner_it } => {
                        builder
                            .with_primary_label(format!("expected `{}`, found `{}`", inner_ea, inner_it))
                            .with_help(format!(
                                "The type annotation says this should be `{}`, but type inference determined it to be `{}`",
                                inner_ea, inner_it
                            ))
                    }
                    ConflictingTypeAnnotationReason::MissingBehaviorImplementation { trait_name, type_name } => {
                        builder
                            .with_primary_label(format!("missing implementation of trait `{}`", trait_name))
                            .with_help(format!("Type `{}` must implement trait `{}`", type_name, trait_name))
                    }
                }
            }
            TypeInferenceErrorKind::UnificationError(unif_err) => {
                match unif_err {
                    crate::hir_ty::UnificationError::TypeMismatch(expected, found) => {
                        builder
                            .with_primary_label(format!("expected `{}`, found `{}`", expected, found))
                            .with_help("These types must be compatible")
                    }
                    crate::hir_ty::UnificationError::OccursCheck(var, ty) => {
                        builder
                            .with_primary_label(format!("type variable `{}` occurs in `{}`", var, ty))
                            .with_help("This would create an infinite type, which is not allowed")
                    }
                }
            }
            TypeInferenceErrorKind::HirResolutionError(err) => match err {
                HirResolutionError::UnresolvedModule { err, .. } => {
                    match err {
                        FqnResolutionError::UnknownRootModule { attempted_module_path } => builder.with_primary_label(format!(
                            "Cannot find module '{}'",
                            attempted_module_path
                                .iter()
                                .map(hir::Name::as_str)
                                .collect::<Vec<_>>()
                                .join("::")
                        ))
                            .with_help("Make sure the module path is correct"),
                        FqnResolutionError::UnknownChildModule { module_id, unknown_child, available_child_modules, } => builder
                            .with_primary_label(format!(
                                "module '{}' does not contain child module '{}'",
                                module_id, unknown_child,
                            ))
                            .with_help(format!(
                                "Module '{}' has the following child modules: {}",
                                module_id, available_child_modules.iter().join(", ")
                            )),
                        FqnResolutionError::MissingLocalName { module_id } => builder
                            .with_primary_label(format!("module '{}' found but missing local name", module_id))
                            .with_help(format!("Trying to import a specific item from module '{}'", module_id)),
                        FqnResolutionError::ExtraSegments { fqn, extra_segments } => {
                            let extra_slug = extra_segments
                                .iter()
                                .map(hir::Name::as_str)
                                .collect::<Vec<_>>()
                                .join("::");

                            builder
                                .with_primary_label(format!("module '{}' found but extra path segments '{}' were not resolved", fqn.module_id, extra_slug))
                                .with_help(format!("Module '{}' was found, but the path segments '{}' could not be resolved within it", fqn.module_id, extra_slug))
                        }
                    }
                }
                HirResolutionError::UnknownExpressionReference { path, module_id, .. } => {
                    let path_str = path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::");
                    let module_slug = builder.format_module_slug(module_id);
                    builder
                        .with_primary_label(format!("cannot find `{}`", path_str))
                        .with_help(format!("No value named `{}` found in module `{}`", path_str, module_slug))
                }
                HirResolutionError::UnknownPatternReference { path, module_id, .. } => {
                    let path_str = path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::");
                    let module_slug = builder.format_module_slug(module_id);
                    builder
                        .with_primary_label(format!("cannot find `{}`", path_str))
                        .with_help(format!("No pattern named `{}` found in module `{}`", path_str, module_slug))
                }
                HirResolutionError::UnknownTypeReference { path, module_id, .. } => {
                    let path_str = path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::");
                    let module_slug = builder.format_module_slug(module_id);
                    builder
                        .with_primary_label(format!("cannot find type `{}`", path_str))
                        .with_help(format!("No type named `{}` found in module `{}`", path_str, module_slug))
                }
                HirResolutionError::UnknownTypeDefinition { path, module_id, .. } => {
                    let path_str = path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::");
                    let module_slug = builder.format_module_slug(module_id);
                    builder
                        .with_primary_label(format!("cannot find type `{}`", path_str))
                        .with_help(format!("No type definition named `{}` found in module `{}`", path_str, module_slug))
                }
                HirResolutionError::UnknownTypeDefinitionVariant { variant_name, .. } => {
                    builder
                        .with_primary_label(format!("variant `{}` not found", variant_name.as_str()))
                        .with_help(format!("No variant named `{}` exists on this type", variant_name.as_str()))
                }
                HirResolutionError::MissingTypeDefinitionVariant { .. } => {
                    builder
                        .with_primary_label("missing variant name")
                        .with_help("Multi-variant types require specifying a variant (e.g., `Type::Variant`)")
                }
                HirResolutionError::UnknownTraitReference { path, module_id, .. } => {
                    let path_str = path.iter().map(|n| n.as_str()).collect::<Vec<_>>().join("::");
                    let module_slug = builder.format_module_slug(module_id);
                    builder
                        .with_primary_label(format!("cannot find trait `{}`", path_str))
                        .with_help(format!("No trait named `{}` found in module `{}`", path_str, module_slug))
                }
                HirResolutionError::UnknownTraitMember { subname, .. } => {
                    builder
                        .with_primary_label(format!("cannot find trait member `{}`", subname.as_str()))
                        .with_help("Check that the trait name is correct and the module is imported")
                }
                HirResolutionError::BoundedTraitReference { .. } => {
                    builder
                        .with_primary_label("bounded trait reference")
                        .with_help("Bounded trait references are not yet fully implemented")
                }
            },
            TypeInferenceErrorKind::MissingTraitMemberImplementation {
                trait_name,
                member_name,
                type_name,
            } => builder
                .with_primary_label(format!(
                    "missing implementation of `{}`",
                    member_name
                ))
                .with_help(format!(
                    "Trait `{}` requires an implementation of `{}` for type `{}`",
                    trait_name, member_name, type_name
                )),
        }
    }
}

// TODO: Duplicate error reporting issue
//
// Currently, when a type annotation conflicts with the inferred type, we generate both:
// 1. ConflictingTypeAnnotation - High-level error comparing annotation vs inferred type
// 2. UnificationError - Low-level error from the unification algorithm attempting to unify them
//
// This results in redundant error messages for the same underlying issue. The ConflictingTypeAnnotation
// is more specific and user-friendly, so ideally we should suppress the UnificationError in this case.
//
// Potential solutions:
// - Short-term: Deduplicate errors based on range after collection
// - Medium-term: Use ResolvedType::Error sentinel to prevent cascading errors
// - Long-term: Implement proper error recovery strategy that tracks which expressions already have errors
//
// For now, having both errors is useful for debugging the type checker itself, but this should be
// addressed once the type system is more stable.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceErrorKind {
    ConflictingTypeAnnotation {
        annotated_type: AnnotatedType,
        inferred_type: ResolvedType,
        reason: ConflictingTypeAnnotationReason,
    },
    UnificationError(crate::hir_ty::UnificationError),
    HirResolutionError(HirResolutionError),
    MissingTraitMemberImplementation {
        trait_name: hir::Name,
        member_name: hir::Name,
        type_name: hir::Name,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConflictingTypeAnnotationReason {
    DirectConflict {
        annotated_type: AnnotatedType,
        inferred_type: ResolvedType,
    },
    MissingBehaviorImplementation {
        trait_name: hir::Name,
        type_name: hir::Name,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceWarning {
    kind: TypeInferenceWarningKind,
    range: TextRange,
}

impl TypeInferenceWarning {
    #[must_use]
    pub fn new(kind: TypeInferenceWarningKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInferenceWarningKind {
    // TODO
}
