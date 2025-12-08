use crate::{resolve_cross_module_type_definition, Fql, TypeResolutionError};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::{ne_vec, NonEmpty};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    Literal(hir::Literal),
    VariableDeclaration,
    Nil,
    Destructure {
        target: Fql<hir::TypeDefinition>,
        args: Vec<Fql<hir::Pattern>>,
    },
    Unit,
    Tuple(NonEmpty<Fql<hir::Pattern>>),
}

pub fn resolve_pattern_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    pat_id: hir::PatternIdx,
) -> Result<Pattern, TypeResolutionError> {
    let source_ref = Fql::new(module_id, pat_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let pat = hir_module.get_pattern(pat_id);

    let pat = match pat {
        hir::Pattern::Literal(lit) => Pattern::Literal(lit.clone()),
        hir::Pattern::Unit => Pattern::Unit,
        hir::Pattern::VariableDeclaration { .. } => Pattern::VariableDeclaration,
        hir::Pattern::Tuple(elements) => {
            let fql_elements = elements.iter().map(|e| Fql::new(module_id, *e)).collect();
            unsafe { Pattern::Tuple(NonEmpty::new_unchecked(fql_elements)) }
        }
        hir::Pattern::Destructure { target, args, .. } => {
            resolve_destructure(db, source_ref, module_id, target, args.clone())?
        }
        hir::Pattern::Nil => Pattern::Nil,
        hir::Pattern::Missing => Pattern::Missing,
    };

    Ok(pat)
}

fn resolve_destructure(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Pattern>,
    module_id: ModuleId,
    target: &hir::Path,
    args: Vec<hir::PatternIdx>,
) -> Result<Pattern, TypeResolutionError> {
    let fql_args = args
        .iter()
        .map(|p| Fql::new(module_id, *p))
        .collect::<Vec<_>>();

    let target = match target {
        hir::Path::ThisModule {
            name: type_name,
            subname,
            scope: target_scope,
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);

            if let Some((type_def_id, type_def)) =
                hir_module.get_type_definition_by_name(type_name, *target_scope)
            {
                // Check if this is a qualified variant (e.g., Option::Some)
                if let Some(variant_name) = subname {
                    if !type_def.kind.has_variant(variant_name) {
                        return Err(TypeResolutionError::UnknownPatternReference {
                            source_ref,
                            module_id,
                            path: ne_vec![type_name.clone(), variant_name.clone()],
                        });
                    }
                }
                Fql::new(module_id, type_def_id)
            } else {
                return Err(TypeResolutionError::UnknownPatternReference {
                    source_ref,
                    module_id,
                    path: ne_vec![type_name.clone()],
                });
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Ok(type_fql) =
                resolve_cross_module_type_definition(db, &fqn, source_ref.clone().into())
            {
                type_fql
            } else {
                return Err(TypeResolutionError::UnknownPatternReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                });
            }
        }
        hir::Path::Unknown(names) => {
            return Err(TypeResolutionError::UnknownPatternReference {
                source_ref,
                module_id,
                path: names.clone(),
            })
        }
    };

    Ok(Pattern::Destructure {
        target,
        args: fql_args,
    })
}
