use crate::{resolve_cross_module_pattern, resolve_cross_module_type_definition, Fql};
use alloy_hir as hir;
use alloy_workspace::ModuleId;
use non_empty_vec::{ne_vec, NonEmpty};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    UnknownReference {
        source_ref: Fql<hir::Pattern>,
        module_id: ModuleId,
        path: NonEmpty<hir::Name>,
    },
    Literal(hir::Literal),
    PatternRef(Fql<hir::Pattern>),
    VariableDeclaration,
    Nil,
    Destructure {
        target: Fql<hir::TypeDefinition>,
        args: Vec<Fql<hir::Pattern>>,
    },
    Unit,
    Tuple(NonEmpty<Fql<hir::Pattern>>),
}

pub fn resolve_pattern(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    pat_id: hir::PatternIdx,
) -> Pattern {
    let source_ref = Fql::new(module_id, pat_id);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let pat = hir_module.get_pattern(pat_id);

    match pat {
        hir::Pattern::Literal(lit) => Pattern::Literal(lit.clone()),
        hir::Pattern::Unit => Pattern::Unit,
        hir::Pattern::VariableDeclaration { .. } => Pattern::VariableDeclaration,
        hir::Pattern::Tuple(elements) => {
            let fql_elements = elements.iter().map(|e| Fql::new(module_id, *e)).collect();
            unsafe { Pattern::Tuple(NonEmpty::new_unchecked(fql_elements)) }
        }
        hir::Pattern::PatternRef { path, .. } => {
            resolve_pattern_ref(db, source_ref, module_id, path)
        }
        hir::Pattern::Destructure { target, args, .. } => {
            resolve_destructure(db, source_ref, module_id, target, args.clone())
        }
        hir::Pattern::Nil => Pattern::Nil,
        hir::Pattern::Missing => Pattern::Missing,
    }
}

fn resolve_pattern_ref(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Pattern>,
    module_id: ModuleId,
    path: &hir::Path,
) -> Pattern {
    match path {
        hir::Path::ThisModule {
            name,
            subname,
            scope: this_scope,
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);

            // First check if this is a qualified variant reference (e.g., Option::None)
            if let Some(subname) = subname {
                let type_name = name;
                let variant_name = subname;

                // Try to find the type definition, starting from the current scope
                // and falling back to the root scope
                let type_def_result =
                    hir_module.get_type_definition_by_name(type_name, *this_scope);

                if let Some((type_def_id, type_def)) = type_def_result {
                    // Check if this type definition has the requested variant
                    if type_def.kind.has_variant(variant_name) {
                        return Pattern::Destructure {
                            target: Fql::new(module_id, type_def_id),
                            args: vec![],
                        };
                    }
                }
            }

            // Fall back to looking up as a pattern reference
            if let Some((pat_id, _)) = hir_module.get_pattern_by_name(name, *this_scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                Pattern::PatternRef(pat_fql)
            } else {
                Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: ne_vec![name.clone()],
                }
            }
        }
        hir::Path::OtherModule(fqn) => {
            // Check if this is a qualified variant
            // Case 1: import Option, then Option::None (sub_path: ["None"])
            // Case 2: import option, then option::Option::None (sub_path: ["Option", "None"])
            if !fqn.sub_path.is_empty() {
                // Try to resolve as a qualified variant
                if let Some(type_def_fql) = resolve_cross_module_type_definition(db, &fqn) {
                    return Pattern::Destructure {
                        target: type_def_fql,
                        args: vec![],
                    };
                }
            }

            // Fall back to pattern reference lookup
            if let Some(pat_fql) = resolve_cross_module_pattern(db, &fqn) {
                Pattern::PatternRef(pat_fql)
            } else {
                Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                }
            }
        }
        hir::Path::Unknown(names) => Pattern::UnknownReference {
            source_ref,
            module_id,
            path: names.clone(),
        },
    }
}

fn resolve_destructure(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Pattern>,
    module_id: ModuleId,
    target: &hir::Path,
    args: Vec<hir::PatternIdx>,
) -> Pattern {
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
                        return Pattern::UnknownReference {
                            source_ref,
                            module_id,
                            path: ne_vec![type_name.clone(), variant_name.clone()],
                        };
                    }
                }
                Fql::new(module_id, type_def_id)
            } else {
                return Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: ne_vec![type_name.clone()],
                };
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(type_fql) = resolve_cross_module_type_definition(db, &fqn) {
                type_fql
            } else {
                return Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: fqn.segments(),
                };
            }
        }
        hir::Path::Unknown(names) => {
            return Pattern::UnknownReference {
                source_ref,
                module_id,
                path: names.clone(),
            }
        }
    };

    Pattern::Destructure {
        target,
        args: fql_args,
    }
}
