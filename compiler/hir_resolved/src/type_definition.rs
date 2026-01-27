use crate::{resolve_cross_module_type_definition, Fql, TypeDefinitionLookup, TypeResolutionError};
use alloy_hir as hir;
use alloy_scope::ScopeIdx;
use alloy_workspace::ModuleId;
use non_empty_vec::ne_vec;
use crate::cross_module_resolver::resolve_cross_module_optional;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: hir::Name,
    pub kind: TypeDefinitionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinitionKind {
    Single(TypeDefinitionMember),
    Union(Vec<TypeDefinitionMember>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinitionMember {
    name: hir::Name,
    properties: Vec<Fql<hir::TypeReference>>,
}

impl TypeDefinitionMember {
    pub fn name(&self) -> &hir::Name {
        &self.name
    }

    pub fn properties(&self) -> &[Fql<hir::TypeReference>] {
        &self.properties
    }
}

pub fn resolve_type_definition_by_path(
    db: &dyn hir::HirDatabase,
    current_module_id: ModuleId,
    path: &hir::Path,
    source_ref: Fql<hir::TypeReference>,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    match path {
        hir::Path::ThisModule { name, scope, .. } => {
            let Some(type_def_fql) =
                get_type_definition_by_name(db, current_module_id, name, *scope)
            else {
                return Err(TypeResolutionError::UnknownTypeDefinition {
                    source_ref,
                    module_id: current_module_id,
                    path: ne_vec![name.clone()],
                });
            };
            Ok(type_def_fql)
        }
        hir::Path::OtherModule(fqn) => {
            resolve_cross_module_type_definition(db, fqn, source_ref.into())
        }
        hir::Path::Unknown(names) => Err(TypeResolutionError::UnknownTypeDefinition {
            source_ref,
            module_id: current_module_id,
            path: names.clone(),
        }),
    }
}

pub(crate) fn resolve_type_definition_by_path_variant(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    path: &hir::Path,
) -> Option<(Fql<hir::TypeDefinition>, hir::Name)> {
    let (type_def_fql, variant_name): (Fql<hir::TypeDefinition>, hir::Name) = match path {
        hir::Path::ThisModule {
            name,
            subname,
            scope,
        } => {
            let variant_name = subname.clone()?;
            let (hir_module, _) = hir::lower_file(db, module_id);
            let (type_def_id, _) = hir_module.get_type_definition_by_name(name, *scope)?;

            (Fql::new(module_id, type_def_id), variant_name)
        }
        hir::Path::OtherModule(fqn) => {
            let variant_name = fqn.sub_path.clone()?;
            let td_fql = resolve_cross_module_optional::<hir::TypeDefinition, TypeDefinitionLookup>(
                db, fqn,
            )?;

            (td_fql, variant_name)
        }
        hir::Path::Unknown(_) => return None,
    };

    Some((type_def_fql, variant_name))
}

fn get_type_definition_by_name(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    name: &hir::Name,
    scope: ScopeIdx,
) -> Option<Fql<hir::TypeDefinition>> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let (type_idx, _) = hir_module.get_type_definition_by_name(name, scope)?;

    Some(Fql::new(module_id, type_idx))
}

#[salsa::tracked]
pub fn resolve_type_definition_by_ref_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    type_idx: hir::TypeIdx,
) -> Result<Fql<hir::TypeDefinition>, TypeResolutionError> {
    let source_ref = Fql::new(module_id, type_idx);
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_ref = hir_module.get_type_reference(type_idx);

    match type_ref {
        hir::TypeReference::Named(path) => {
            resolve_type_definition_by_path(db, module_id, path, source_ref)
        }
        hir::TypeReference::Bounded { base, args: _TODO } => {
            resolve_type_definition_by_ref_id(db, module_id, *base)
        }
        _ => unreachable!(
            "Invalid type reference for trait resolution: {:?}",
            type_ref
        ),
    }
}

#[salsa::tracked]
pub fn resolve_type_definition_by_id(
    db: &dyn hir::HirDatabase,
    module_id: ModuleId,
    type_def_idx: hir::TypeDefinitionIdx,
) -> Option<TypeDefinition> {
    let (hir_module, _) = hir::lower_file(db, module_id);
    let type_def = hir_module.get_type_definition(type_def_idx);

    let kind = match &type_def.kind {
        hir::TypeDefinitionKind::Single(member) => {
            let resolved_member = TypeDefinitionMember {
                name: member.name().clone(),
                properties: member
                    .properties()
                    .iter()
                    .map(|type_ref_idx| Fql::new(module_id, *type_ref_idx))
                    .collect(),
            };
            TypeDefinitionKind::Single(resolved_member)
        }
        hir::TypeDefinitionKind::Union(members) => {
            let resolved_members = members
                .iter()
                .map(|member| TypeDefinitionMember {
                    name: member.name().clone(),
                    properties: member
                        .properties()
                        .iter()
                        .map(|type_ref_idx| Fql::new(module_id, *type_ref_idx))
                        .collect(),
                })
                .collect();
            TypeDefinitionKind::Union(resolved_members)
        }
        hir::TypeDefinitionKind::Missing | hir::TypeDefinitionKind::TypeVariable(_) => {
            return None; // or handle as needed
        }
    };

    Some(TypeDefinition {
        name: type_def.name.clone(),
        kind,
    })
}
