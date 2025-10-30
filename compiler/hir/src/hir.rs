use super::{Fqn, HirDatabase, Name};
use crate::ast_glossary::AstGlossary;
use crate::index::{Index, IndexItem};
use std::sync::Arc;

use alloy_ast as ast;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_syntax::SyntaxElement;
use ast::AstElement;
use la_arena::Idx;
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;
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

pub use module::*;

mod path;

pub use path::*;

mod pattern;

pub use pattern::*;

mod source_file;

pub use source_file::*;

mod statement;

pub use statement::*;

mod r#trait;

pub use r#trait::*;

mod type_reference;

pub use type_reference::*;

mod type_annotation;

pub use type_annotation::*;

mod type_variable;

pub use type_variable::*;

mod type_definition;

pub use type_definition::*;

mod value;

pub use value::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum HirReferenceType {
    Expression,
    Pattern,
    Type,
}

#[allow(clippy::module_name_repetitions)]
#[salsa::tracked]
pub struct HirModule {
    pub imports: Arc<Index<Import>>,
    pub expressions: Arc<Index<Expression>>,
    pub patterns: Arc<Index<Pattern>>,
    pub type_references: Arc<Index<TypeReference>>,
    pub type_definitions: Arc<Index<TypeDefinition>>,
    pub scopes: Arc<Scopes>,
    pub warnings: Vec<LoweringWarning>,
    pub errors: Vec<LoweringError>,
}

// impl HirModule {
//     pub fn imports(&self) -> impl Iterator<Item = IndexItem<Import>> {
//         self.imports.iter()
//     }
//
//     pub fn expressions(&self) -> impl Iterator<Item = IndexItem<Expression>> {
//         self.expressions.iter()
//     }
//
//     #[must_use]
//     pub fn get_expression(&self, id: ExpressionIdx) -> &Expression {
//         self.expressions.get(id)
//     }
//
//     #[must_use]
//     pub fn get_expression_by_name(
//         &self,
//         name: &Name,
//         scope: ScopeIdx,
//     ) -> Option<(ExpressionIdx, &Expression)> {
//         self.expressions.get_by_scoped_name(name, scope)
//     }
//
//     pub fn patterns(&self) -> impl Iterator<Item = IndexItem<Pattern>> {
//         self.patterns.iter()
//     }
//
//     #[must_use]
//     pub fn get_pattern_by_name(
//         &self,
//         name: &Name,
//         scope: ScopeIdx,
//     ) -> Option<(PatternIdx, &Pattern)> {
//         self.patterns.get_by_scoped_name(name, scope)
//     }
//
//     #[must_use]
//     pub fn get_pattern(&self, id: PatternIdx) -> &Pattern {
//         self.patterns.get(id)
//     }
//
//     pub fn type_references(&self) -> impl Iterator<Item = IndexItem<TypeReference>> {
//         self.type_references.iter()
//     }
//
//     #[must_use]
//     pub fn get_type_reference_by_name(
//         &self,
//         name: &Name,
//         scope: ScopeIdx,
//     ) -> Option<(TypeIdx, &TypeReference)> {
//         self.type_references.get_by_scoped_name(name, scope)
//     }
//
//     pub fn type_definitions(&self) -> impl Iterator<Item = IndexItem<TypeDefinition>> {
//         self.type_definitions.iter()
//     }
//
//     #[must_use]
//     pub fn warnings(&self) -> &[LoweringWarning] {
//         &self.warnings
//     }
//
//     #[must_use]
//     pub fn errors(&self) -> &[LoweringError] {
//         &self.errors
//     }
// }

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoweringError {
    kind: LoweringErrorKind,
    range: TextRange,
}

impl LoweringError {
    #[must_use]
    pub fn new(kind: LoweringErrorKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LoweringErrorKind {
    ConflictingValue {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ImportGroupNotAtEnd {
        group_range: TextRange,
        position: usize,
        num_segments: usize,
    },
    ConflictingTypeAnnotationName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingTypeDefinitionName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingTypeVariableName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    UnknownReference {
        reference: Name,
        reference_type: HirReferenceType,
        path: NonEmpty<Name>,
        current_scope: ScopeIdx,
    },
    MultipleSelfTypeVariablesInTraitDefinition {
        trait_name: Name,
        ranges: Vec<TextRange>,
    },
    NumberLiteralTooLarge,
    CharLiteralInvalid,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoweringWarning {
    kind: LoweringWarningKind,
    range: TextRange,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LoweringWarningKind {
    DuplicateImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    UnusedImport {
        import: Import,
    },
}

struct LoweringCtx<'db> {
    db: &'db dyn HirDatabase,
    glossary: AstGlossary,
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_references: Index<TypeReference>,
    type_definitions: Index<TypeDefinition>,
    scopes: Scopes,
    used_imports: HashSet<ImportIdx>,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl<'db> LoweringCtx<'db> {
    fn new(db: &'db dyn HirDatabase, glossary: AstGlossary) -> Self {
        Self {
            db,
            glossary,
            imports: Index::new(),
            expressions: Index::new(),
            patterns: Index::new(),
            type_references: Index::new(),
            type_definitions: Index::new(),
            scopes: Scopes::default(),
            used_imports: HashSet::new(),
            warnings: vec![],
            errors: vec![],
        }
    }

    fn finish(self) -> HirModule {
        let mut warnings = self.warnings;

        for (id, import, _, _) in self.imports.iter() {
            if !self.used_imports.contains(&id) {
                let warn = LoweringWarningKind::UnusedImport { import: *import };
                warnings.push(LoweringWarning {
                    kind: warn,
                    range: self.imports.get_range(id),
                });
            }
        }

        HirModule::new(
            self.db,
            self.imports.into(),
            self.expressions.into(),
            self.patterns.into(),
            self.type_references.into(),
            self.type_definitions.into(),
            self.scopes.into(),
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
                self.resolve_type_reference(path_segments, path_range, reference_type)
            }
        }
    }

    fn resolve_type_reference(
        &mut self,
        path_segments: &[String],
        path_range: TextRange,
        reference_type: HirReferenceType,
    ) -> Option<Path> {
        if let [first, rest @ ..] = path_segments {
            let local_name = Name::new(first);

            if let Some(_tid) = self.type_definitions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first));
            }
            if let Some(_tid) = self.type_references.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first));
            }
            if let Some(_ast) = self.glossary.get_type_definition_by_name(first) {
                return Some(Path::this_module(rest, first));
            }
            if let Some(import_id) = self.imports.get_id(&local_name, &self.scopes) {
                self.used_imports.insert(import_id);

                let import = self.imports.get(import_id);
                let fqn = Fqn::new(
                    import.segments(self.db).iter().cloned(),
                    local_name.clone(),
                    rest.to_vec(),
                );

                return Some(Path::OtherModule(fqn));
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
        Path::Unknown(segments)
    }

    fn resolve_expression_reference(
        &mut self,
        path_segments: &[String],
        path_range: TextRange,
        reference_type: HirReferenceType,
    ) -> Option<Path> {
        if let [first, rest @ ..] = path_segments {
            let local_name = Name::new(first);

            if let Some(_pid) = self.patterns.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first));
            }
            if let Some(_eid) = self.expressions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first));
            }
            if let Some(_ast) = self.glossary.get_value_by_name(first) {
                return Some(Path::this_module(rest, first));
            }
            return self.resolve_type_reference(path_segments, path_range, reference_type);
        }

        None
    }

    pub(crate) fn add_expression(
        &mut self,
        expression: Expression,
        element: &SyntaxElement,
    ) -> ExpressionIdx {
        self.expressions
            .insert_not_named(expression, element.text_range())
    }

    pub(crate) fn add_missing_expression(&mut self, element: &SyntaxElement) -> ExpressionIdx {
        self.add_expression(Expression::Missing, element)
    }

    pub(crate) fn add_value(
        &mut self,
        name: Name,
        expression: Expression,
        element: &SyntaxElement,
    ) -> ExpressionIdx {
        let res =
            self.expressions
                .insert_named(name, expression, element.text_range(), &self.scopes);

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingValue {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.text_range());

                self.add_missing_expression(element)
            }
            Ok(id) => id,
        }
    }

    pub(crate) fn add_pattern(&mut self, pattern: Pattern, element: &SyntaxElement) -> PatternIdx {
        match &pattern {
            Pattern::VariableDeclaration { name } => {
                let res = self.patterns.insert_named(
                    name.clone(),
                    pattern,
                    element.text_range(),
                    &self.scopes,
                );

                match res {
                    Err(err) => {
                        let err = LoweringErrorKind::ConflictingValue {
                            name: err.name,
                            first: err.first,
                            second: err.second,
                        };
                        self.error(err, element.text_range());

                        self.add_missing_pattern(element)
                    }
                    Ok(pid) => pid,
                }
            }
            _ => self
                .patterns
                .insert_not_named(pattern, element.text_range()),
        }
    }

    pub(crate) fn add_missing_pattern(&mut self, element: &SyntaxElement) -> PatternIdx {
        self.add_pattern(Pattern::Missing, element)
    }

    pub(crate) fn add_type_reference(
        &mut self,
        type_: TypeReference,
        element: &SyntaxElement,
    ) -> TypeIdx {
        self.type_references
            .insert_not_named(type_, element.text_range())
    }

    pub(crate) fn add_missing_type_reference(&mut self, element: &SyntaxElement) -> TypeIdx {
        self.add_type_reference(TypeReference::Missing, element)
    }

    pub(crate) fn add_type_annotation(
        &mut self,
        name: Name,
        type_id: TypeIdx,
        element: &SyntaxElement,
    ) -> TypeIdx {
        let res = self
            .type_references
            .add_name(name, type_id, element.text_range(), &self.scopes);

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingTypeAnnotationName {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.text_range());

                self.add_missing_type_reference(element)
            }
            Ok(pid) => pid,
        }
    }

    pub(crate) fn add_type_definition(
        &mut self,
        type_definition: TypeDefinition,
        element: &SyntaxElement,
    ) {
        let res = self.type_definitions.insert_named(
            type_definition.name.clone(),
            type_definition,
            element.text_range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingTypeDefinitionName {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.text_range());
        }
    }

    pub(crate) fn add_type_variable(
        &mut self,
        name: String,
        type_variable: TypeVariable,
        element: &SyntaxElement,
    ) -> TypeDefinitionIdx {
        let name = Name::new(name);
        let type_definition = TypeDefinition {
            name: name.clone(),
            kind: TypeDefinitionKind::TypeVariable(type_variable),
        };

        let res = self.type_definitions.insert_named(
            name.clone(),
            type_definition.clone(),
            element.text_range(),
            &self.scopes,
        );

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingTypeVariableName {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, element.text_range());

                self.type_definitions
                    .insert_not_named(type_definition, element.text_range())
            }
            Ok(pid) => pid,
        }
    }

    pub(crate) fn add_trait(&mut self, name: Name, trait_: Trait, element: &SyntaxElement) {
        let _ = self.type_definitions.insert_named(
            name.clone(),
            TypeDefinition {
                name,
                kind: TypeDefinitionKind::Trait(trait_),
            },
            element.text_range(),
            &self.scopes,
        );
    }

    pub(crate) fn add_import(&mut self, segments: &NonEmpty<Name>, element: &SyntaxElement) {
        let (last, path) = segments.split_last();
        let new_import = Import::new(self.db, path.to_vec(), last.clone());

        if let Some(existing_id) = self.imports.get_id(last, &self.scopes) {
            let existing_import = self.imports.get(existing_id);
            let existing_import_range = self.imports.get_range(existing_id);

            if &new_import == existing_import {
                let warn = LoweringWarningKind::DuplicateImport {
                    name: last.clone(),
                    first: existing_import_range,
                    second: element.text_range(),
                };
                self.warning(warn, element.text_range());

                return;
            }
        }

        let res = self.imports.insert_named(
            last.clone(),
            new_import.clone(),
            element.text_range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingImport {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.text_range());
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

#[must_use]
pub fn lower_source_file(db: &dyn HirDatabase, source_file: &ast::SourceFile) -> HirModule {
    let glossary = AstGlossary::summarize_source_file(source_file);

    let mut ctx = LoweringCtx::new(db, glossary);
    source_file::lower_source_file(&mut ctx, source_file);
    ctx.finish()
}

impl<'db> LoweringCtx<'db> {
    fn warning(&mut self, kind: LoweringWarningKind, range: TextRange) {
        self.warnings.push(LoweringWarning { kind, range });
    }

    fn error(&mut self, kind: LoweringErrorKind, range: TextRange) {
        self.errors.push(LoweringError { kind, range });
    }
}
