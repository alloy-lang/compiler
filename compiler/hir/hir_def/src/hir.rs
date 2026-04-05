use super::*;
use crate::ast_glossary::AstGlossary;

use alloy_ast as ast;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_workspace::ModuleId;
use ast::AstElement;
use la_arena::Idx;
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;
use rustc_hash::FxHashMap;
use std::collections::HashSet;
use std::convert::TryFrom;
use text_size::TextRange;

mod behavior;

pub use behavior::*;

mod expression;

pub use expression::*;

mod import;

pub use import::*;

mod module;

use module::*;

mod path;

pub use path::*;

mod pattern;

pub use pattern::*;

mod source_file;

mod r#trait;

pub use r#trait::*;

mod type_reference;

pub use type_reference::*;

mod type_annotation;

use type_annotation::*;

mod type_variable;

pub use type_variable::*;

mod type_definition;

pub use type_definition::*;

mod value;

pub use value::*;

use crate::fqn::{Fqn, FqnResolutionError};
use crate::index::Index;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum HirReferenceType {
    Expression,
    Pattern,
    Type,
}

pub(crate) struct LoweringCtx<'db> {
    #[allow(dead_code)]
    db: &'db dyn HirDefDatabase,
    glossary: AstGlossary,
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_references: Index<TypeReference>,
    type_definitions: Index<TypeDefinition>,
    type_variables: Index<TypeVariable>,
    traits: Index<Trait>,
    behaviors: Index<Behavior, (/* type */ TypeIdx, /* trait */ TypeIdx)>,
    scopes: Scopes,
    used_imports: HashSet<ImportIdx>,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl<'db> LoweringCtx<'db> {
    pub(crate) fn new(db: &'db dyn HirDefDatabase, glossary: AstGlossary) -> Self {
        Self {
            db,
            glossary,
            imports: Index::new(),
            expressions: Index::new(),
            patterns: Index::new(),
            type_references: Index::new(),
            type_definitions: Index::new(),
            type_variables: Index::new(),
            traits: Index::new(),
            behaviors: Index::new(),
            scopes: Scopes::default(),
            used_imports: HashSet::new(),
            warnings: vec![],
            errors: vec![],
        }
    }

    pub(crate) fn finish(self) -> HirModule {
        let mut warnings = self.warnings;

        for (id, import, _, _) in self.imports.iter() {
            if !self.used_imports.contains(&id) {
                let warn = LoweringWarningKind::UnusedImport {
                    import: import.clone(),
                };
                warnings.push(LoweringWarning::new(warn, self.imports.get_range(id)));
            }
        }

        let mut value_definitions: FxHashMap<ExpressionIdx, ValueDefinition> = Default::default();
        let mut matched_type_annotations: HashSet<TypeIdx> = HashSet::new();
        for (expression_id, _, _, name, scope) in self.expressions.iter_by_scope(Scopes::ROOT) {
            let value_definition =
                match self
                    .type_references
                    .get_by_scoped_name(&name, scope, &self.scopes)
                {
                    None => ValueDefinition {
                        name,
                        expr_idx: expression_id,
                        type_annotation: None,
                    },
                    Some((ta, _)) => {
                        matched_type_annotations.insert(ta);
                        ValueDefinition {
                            name,
                            expr_idx: expression_id,
                            type_annotation: Some(ta),
                        }
                    }
                };

            value_definitions.insert(expression_id, value_definition);
        }

        for (type_id, _, range, name, _) in self.type_references.iter_by_scope(Scopes::ROOT) {
            if !matched_type_annotations.contains(&type_id) {
                warnings.push(LoweringWarning::new(
                    LoweringWarningKind::MissingDefinition { name },
                    range,
                ));
            }
        }

        HirModule::new(
            self.imports,
            self.expressions,
            self.patterns,
            self.type_references,
            self.type_definitions,
            self.type_variables,
            self.traits,
            self.behaviors,
            value_definitions,
            self.scopes,
            warnings,
            self.errors,
        )
    }

    pub(crate) fn resolve_reference_path(
        &mut self,
        ast_path: &ast::Path,
        reference_type: HirReferenceType,
    ) -> Option<Path> {
        self.resolve_reference_segments(&ast_path.segments(), ast_path.range(), reference_type)
    }

    fn resolve_reference_segments(
        &mut self,
        path_segments: &[String],
        path_range: TextRange,
        reference_type: HirReferenceType,
    ) -> Option<Path> {
        match reference_type {
            HirReferenceType::Expression => {
                self.resolve_expression_reference(path_segments, path_range, reference_type)
            }
            HirReferenceType::Pattern | HirReferenceType::Type => {
                self.resolve_type_reference(path_segments, path_range, reference_type, &[])
            }
        }
    }

    fn resolve_type_reference(
        &mut self,
        path_segments: &[String],
        path_range: TextRange,
        reference_type: HirReferenceType,
        resolution_kinds: &[ResolutionKind],
    ) -> Option<Path> {
        if let [first, rest @ ..] = path_segments {
            let local_name = Name::new(first);

            if let Some((tid, scope)) = self.traits.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    scope,
                    ResolutionKind::Trait,
                    ResolutionIdx::Trait(tid),
                ));
            }
            if let Some((tid, _scope)) = self.type_definitions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    Scopes::ROOT,
                    ResolutionKind::TypeDefinition,
                    ResolutionIdx::TypeDefinition(tid),
                ));
            }
            if let Some((tid, scope)) = self.type_variables.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    scope,
                    ResolutionKind::TypeVariable,
                    ResolutionIdx::TypeVariable(tid),
                ));
            }
            if let Some((tid, scope)) = self.type_references.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    scope,
                    ResolutionKind::AbstractTraitMember,
                    ResolutionIdx::AbstractTraitMember(tid),
                ));
            }
            if let Some(_ast) = self.glossary.get_type_definition_by_name(first) {
                return Some(Path::this_module(
                    rest,
                    first,
                    Scopes::ROOT,
                    ResolutionKind::TypeDefinition,
                    ResolutionIdx::Unresolved,
                ));
            }
            if let Some((import_id, _)) = self.imports.get_id(&local_name, &self.scopes) {
                self.used_imports.insert(import_id);

                let import = self.imports.get(import_id);
                let fqn = Fqn::resolve(
                    self.db,
                    import.segments().iter().cloned(),
                    local_name.clone(),
                    rest.to_vec(),
                );

                return match fqn {
                    Ok(fqn) => {
                        let resolution_kinds = [
                            resolution_kinds,
                            &[ResolutionKind::Trait, ResolutionKind::TypeDefinition],
                        ]
                        .concat();

                        Some(Path::OtherModule(fqn, resolution_kinds))
                    }
                    Err(err) => {
                        let path =
                            self.report_failed_module_resolution(path_range, reference_type, err);

                        Some(path)
                    }
                };
            }

            let path =
                self.report_unknown_reference(path_range, reference_type, first, rest, local_name);

            return Some(path);
        }

        None
    }

    fn report_unknown_reference(
        &mut self,
        path_range: TextRange,
        reference_type: HirReferenceType,
        first: &String,
        rest: &[String],
        local_name: Name,
    ) -> Path {
        let segments = NonEmpty::from((
            Name::new(first),
            rest.iter().map(Name::new).collect::<Vec<_>>(),
        ));

        self.error(
            LoweringErrorKind::UnknownReference {
                reference: local_name,
                reference_type,
                path: segments.clone(),
                current_scope: self.scopes.current_scope(),
            },
            path_range,
        );
        Path::UnknownReference(segments)
    }

    fn report_failed_module_resolution(
        &mut self,
        path_range: TextRange,
        reference_type: HirReferenceType,
        err: FqnResolutionError,
    ) -> Path {
        self.error(
            LoweringErrorKind::FailedModuleResolution {
                reference_type,
                fqn_error: err.clone(),
            },
            path_range,
        );
        Path::UnresolvedModule(err)
    }

    fn resolve_expression_reference(
        &mut self,
        path_segments: &[String],
        path_range: TextRange,
        reference_type: HirReferenceType,
    ) -> Option<Path> {
        if let [first, rest @ ..] = path_segments {
            let local_name = Name::new(first);

            if let Some((pid, scope)) = self.patterns.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    scope,
                    ResolutionKind::Pattern,
                    ResolutionIdx::Pattern(pid),
                ));
            }
            if let Some((eid, scope)) = self.expressions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(
                    rest,
                    first,
                    scope,
                    ResolutionKind::Expression,
                    ResolutionIdx::Expression(eid),
                ));
            }
            if let Some(_ast) = self.glossary.get_value_by_name(first) {
                return Some(Path::this_module(
                    rest,
                    first,
                    Scopes::ROOT,
                    ResolutionKind::Expression,
                    ResolutionIdx::Unresolved,
                ));
            }
            return self.resolve_type_reference(
                path_segments,
                path_range,
                reference_type,
                &[ResolutionKind::Pattern, ResolutionKind::Expression],
            );
        }

        None
    }

    pub(crate) fn add_expression(
        &mut self,
        expression: Expression,
        element: &impl AstElement,
    ) -> ExpressionIdx {
        self.expressions
            .insert_not_named(expression, element.range())
    }

    pub(crate) fn add_missing_expression(&mut self, element: &impl AstElement) -> ExpressionIdx {
        self.add_expression(Expression::Missing, element)
    }

    pub(crate) fn add_value(
        &mut self,
        name: Name,
        expression: Expression,
        element: &impl AstElement,
    ) -> ExpressionIdx {
        // TODO: track expression and value ranges separately
        let res = self
            .expressions
            .insert_named(name, expression, element.range(), &self.scopes);

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingValue {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.range());

                self.add_missing_expression(element)
            }
            Ok(id) => id,
        }
    }

    pub(crate) fn add_pattern(
        &mut self,
        pattern: Pattern,
        element: &impl AstElement,
    ) -> PatternIdx {
        match &pattern {
            Pattern::VariableDeclaration { name } => {
                let res = self.patterns.insert_named(
                    name.clone(),
                    pattern,
                    element.range(),
                    &self.scopes,
                );

                match res {
                    Err(err) => {
                        let err = LoweringErrorKind::ConflictingValue {
                            name: err.name,
                            first: err.first,
                            second: err.second,
                        };
                        self.error(err, element.range());

                        self.add_missing_pattern(element)
                    }
                    Ok(pid) => pid,
                }
            }
            _ => self.patterns.insert_not_named(pattern, element.range()),
        }
    }

    pub(crate) fn add_missing_pattern(&mut self, element: &impl AstElement) -> PatternIdx {
        self.add_pattern(Pattern::Missing, element)
    }

    pub(crate) fn add_type_reference(
        &mut self,
        type_: TypeReference,
        element: &impl AstElement,
    ) -> TypeIdx {
        self.type_references
            .insert_not_named(type_, element.range())
    }

    pub(crate) fn add_missing_type_reference(&mut self, element: &impl AstElement) -> TypeIdx {
        self.add_type_reference(TypeReference::Missing, element)
    }

    pub(crate) fn add_type_annotation(
        &mut self,
        name: Name,
        type_id: TypeIdx,
        element: &impl AstElement,
    ) -> TypeIdx {
        // TODO: track type annotation and type reference ranges separately
        let res = self
            .type_references
            .add_name(name, type_id, element.range(), &self.scopes);

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingTypeAnnotationName {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.range());

                self.add_missing_type_reference(element)
            }
            Ok(pid) => pid,
        }
    }

    pub(crate) fn add_type_definition(
        &mut self,
        type_definition: TypeDefinition,
        element: &impl AstElement,
    ) {
        let res = self.type_definitions.insert_named(
            type_definition.name.clone(),
            type_definition,
            element.range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingTypeDefinitionName {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.range());
        }
    }

    pub(crate) fn add_type_variable(
        &mut self,
        name: String,
        type_variable: TypeVariableKind,
        element: &impl AstElement,
    ) -> TypeVariableIdx {
        let name = Name::new(name);
        let type_variable = TypeVariable {
            name: name.clone(),
            kind: type_variable,
        };

        let res = self.type_variables.insert_named(
            name.clone(),
            type_variable.clone(),
            element.range(),
            &self.scopes,
        );

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingTypeVariableName {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.range());

                self.type_variables
                    .insert_not_named(type_variable, element.range())
            }
            Ok(pid) => pid,
        }
    }

    pub(crate) fn add_trait(&mut self, trait_: Trait, element: &impl AstElement) {
        let res =
            self.traits
                .insert_named(trait_.name.clone(), trait_, element.range(), &self.scopes);

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingTraitDefinitionName {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.range());
        }
    }

    pub(crate) fn add_behavior(&mut self, behavior: Behavior, element: &impl AstElement) {
        let res = self.behaviors.insert_named(
            (behavior.attached_type, behavior.attached_trait),
            behavior.clone(),
            element.range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingBehaviorDefinition {
                type_: behavior.attached_type,
                trait_: behavior.attached_trait,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.range());
        }
    }

    pub(crate) fn add_import(&mut self, segments: &NonEmpty<Name>, element: &impl AstElement) {
        let new_import = Import::new(segments);
        let last = segments.last();

        if let Some((existing_id, _)) = self.imports.get_id(last, &self.scopes) {
            let existing_import = self.imports.get(existing_id);
            let existing_import_range = self.imports.get_range(existing_id);

            if &new_import == existing_import {
                let warn = LoweringWarningKind::DuplicateImport {
                    name: last.clone(),
                    first: existing_import_range,
                    second: element.range(),
                };
                self.warning(warn, element.range());

                return;
            }
        }

        let res = self.imports.insert_named(
            last.clone(),
            new_import.clone(),
            element.range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingImport {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.range());
        }
    }
}

impl<'db> LoweringCtx<'db> {
    fn inside_scope<F, R>(&mut self, tag: &str, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.scopes.push_scope(tag);

        let res = f(self);

        self.scopes.pop_scope();

        res
    }
}

impl<'db> LoweringCtx<'db> {
    fn warning(&mut self, kind: LoweringWarningKind, range: TextRange) {
        self.warnings.push(LoweringWarning::new(kind, range));
    }

    fn error(&mut self, kind: LoweringErrorKind, range: TextRange) {
        self.errors.push(LoweringError::new(kind, range));
    }
}

/// Lower a raw source file to HIR.
/// This query is cached by salsa, so repeated calls with the same file
/// will return the cached result unless the file contents have changed.
/// Returns both parse errors and the HIR module.
#[salsa::tracked]
pub fn lower_file<'db>(
    db: &'db dyn HirDefDatabase,
    module_id: ModuleId,
) -> (HirModule, Vec<alloy_parser::ParseError>) {
    let (source_file, parse_errors) = ast::parse_source_file(db, module_id);
    let Some(source_file) = source_file else {
        return (HirModule::empty(), parse_errors);
    };

    let hir_module = lower_source_file(db, &source_file);
    (hir_module, parse_errors)
}

#[salsa::tracked]
pub fn module_value_def(
    db: &dyn HirDefDatabase,
    module_id: ModuleId,
    expr_idx: ExpressionIdx,
) -> Option<ValueDef<'_>> {
    let (hir_module, _) = lower_file(db, module_id);

    hir_module
        .get_value_by_id(&expr_idx)
        .map(|v| ValueDef::new(db, module_id, v.name.clone(), v.type_annotation, v.expr_idx))
}

#[must_use]
fn lower_source_file<'db>(
    db: &'db dyn HirDefDatabase,
    source_file: &'db ast::SourceFile,
) -> HirModule {
    let glossary = AstGlossary::summarize_source_file(source_file);

    let mut ctx = LoweringCtx::new(db, glossary);
    source_file::lower_source_file(&mut ctx, source_file);
    ctx.finish()
}

#[cfg(test)]
mod tests {
    use crate::tests::TestHirDefDatabase;
    use crate::{Expression, Name, Path, ResolutionIdx, ResolutionKind};
    use alloy_scope::Scopes;
    use alloy_test_harness::idx;
    use alloy_workspace::WorkspaceDatabase;

    #[test]
    fn salsa_caching_is_correctly_invalidated_when_module_is_updated() {
        let module_path = camino::Utf8Path::new("./test/test.alloy");
        let module_slug = "test";

        let mut db = TestHirDefDatabase::default();
        let module_id = db.add_module(
            module_slug,
            module_path,
            r#"
                let string_result = "hi"
            "#,
        );

        let (first_hir_module, _) = crate::lower_file(&db, module_id);

        db.add_module(
            module_slug,
            module_path,
            r#"
                let string_result = "hi"
                let int_result = 42
            "#,
        );

        let (second_hir_module, _) = crate::lower_file(&db, module_id);

        assert_ne!(first_hir_module, second_hir_module);
    }

    #[test]
    fn resolve_same_module_function_call_trait_reference() {
        let mut db = TestHirDefDatabase::default();
        let module_id = db.add_module(
            "test_stuff",
            camino::Utf8Path::new("./test_stuff.alloy"),
            r"
    trait TestTrait where
      self = #Type[_]
      typeof test_func : (a -> self[b]) -> self[a] -> self[b] where
        typevar a
        typevar b
    end

    typeof example : (t1 -> m[t2]) -> m[t1] -> m[t2] where
      typevar m = TestTrait
      typevar t1
      typevar t2
    let example = TestTrait::test_func
",
        );

        let (first_hir_module, _) = crate::lower_file(&db, module_id);
        let (_, expr) = first_hir_module
            .get_expression_by_name(&Name::new("example"), Scopes::ROOT)
            .expect("must find example expression");

        assert_eq!(
            &Expression::VariableRef {
                path: Path::ThisModule {
                    name: Name::new("TestTrait"),
                    subname: Some(Name::new("test_func")),
                    scope: Scopes::ROOT,
                    resolution_kind: ResolutionKind::Trait,
                    resolution_idx: ResolutionIdx::Trait(idx!(0)),
                },
                scope: Scopes::ROOT,
            },
            expr
        );
    }
}
