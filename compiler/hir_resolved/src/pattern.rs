use crate::{resolve_cross_module_pattern, resolve_cross_module_type_definition, Fql};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::NonEmpty;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    UnknownReference {
        source_ref: Fql<hir::Pattern>,
        module_id: ModuleId,
        path: hir::Path,
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
        hir::Pattern::PatternRef { path, scope } => {
            resolve_pattern_ref(db, source_ref, module_id, path, *scope)
        }
        hir::Pattern::Destructure {
            target,
            scope,
            args,
        } => resolve_destructure(db, source_ref, module_id, target, *scope, args.clone()),
        hir::Pattern::Nil => Pattern::Nil,
        hir::Pattern::Missing => Pattern::Missing,
    }
}

fn resolve_pattern_ref(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Pattern>,
    module_id: ModuleId,
    path: &hir::Path,
    scope: ScopeIdx,
) -> Pattern {
    match path {
        hir::Path::ThisModule {
            path: names,
            scope: _, // check to see if the scope is the same as the expression's scope
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((pat_id, _)) = hir_module.get_pattern_by_name(names.last(), scope) {
                let pat_fql = Fql::new(module_id, pat_id);
                Pattern::PatternRef(pat_fql)
            } else {
                Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: path.clone(),
                }
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(pat_fql) = resolve_cross_module_pattern(db, &fqn) {
                Pattern::PatternRef(pat_fql)
            } else {
                Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: path.clone(),
                }
            }
        }
        hir::Path::Unknown(_) => Pattern::UnknownReference {
            source_ref,
            module_id,
            path: path.clone(),
        },
    }
}

fn resolve_destructure(
    db: &dyn hir::HirDatabase,
    source_ref: Fql<hir::Pattern>,
    module_id: ModuleId,
    target: &hir::Path,
    scope: ScopeIdx,
    args: Vec<hir::PatternIdx>,
) -> Pattern {
    let fql_args = args
        .iter()
        .map(|p| Fql::new(module_id, *p))
        .collect::<Vec<_>>();

    let target = match target {
        hir::Path::ThisModule {
            path: names,
            scope: _, // check to see if the scope is the same as the expression's scope
        } => {
            let (hir_module, _) = hir::lower_file(db, module_id);
            if let Some((type_def_id, _)) =
                hir_module.get_type_definition_by_name(names.last(), scope)
            {
                Fql::new(module_id, type_def_id)
            } else {
                return Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: target.clone(),
                };
            }
        }
        hir::Path::OtherModule(fqn) => {
            if let Some(expr_fql) = resolve_cross_module_type_definition(db, &fqn) {
                expr_fql
            } else {
                return Pattern::UnknownReference {
                    source_ref,
                    module_id,
                    path: target.clone(),
                };
            }
        }
        hir::Path::Unknown(_) => {
            return Pattern::UnknownReference {
                source_ref,
                module_id,
                path: target.clone(),
            }
        }
    };

    Pattern::Destructure {
        target,
        args: fql_args,
    }
}
