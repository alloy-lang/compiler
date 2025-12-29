use super::{Fqn, HirDatabase, Name};
use crate::ast_glossary::AstGlossary;
use crate::index::{Index, IndexItem};

use alloy_ast as ast;
use alloy_scope::{ScopeIdx, Scopes};
use alloy_syntax::SyntaxElement;
use alloy_workspace::{ModuleId, SourceFile};
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

use value::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum HirReferenceType {
    Expression,
    Pattern,
    Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_references: Index<TypeReference>,
    type_definitions: Index<TypeDefinition>,
    traits: Index<Trait>,
    behaviors: Index<Behavior, (TypeIdx, TypeIdx)>,
    scopes: Scopes,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl HirModule {
    pub fn warnings(&self) -> &[LoweringWarning] {
        &self.warnings
    }

    pub fn errors(&self) -> &[LoweringError] {
        &self.errors
    }

    fn empty() -> Self {
        Self {
            imports: Default::default(),
            expressions: Default::default(),
            patterns: Default::default(),
            type_references: Default::default(),
            type_definitions: Default::default(),
            traits: Default::default(),
            behaviors: Default::default(),
            scopes: Default::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    // Iterator methods for accessing collections
    pub fn type_definitions(&'_ self) -> impl Iterator<Item = IndexItem<'_, TypeDefinition, Name>> {
        self.type_definitions.iter()
    }

    pub fn expressions(&'_ self) -> impl Iterator<Item = IndexItem<'_, Expression, Name>> {
        self.expressions.iter()
    }

    pub fn patterns(&'_ self) -> impl Iterator<Item = IndexItem<'_, Pattern, Name>> {
        self.patterns.iter()
    }

    pub fn type_references(&'_ self) -> impl Iterator<Item = IndexItem<'_, TypeReference, Name>> {
        self.type_references.iter()
    }

    pub fn imports(&'_ self) -> impl Iterator<Item = IndexItem<'_, Import, Name>> {
        self.imports.iter()
    }

    pub fn traits(&'_ self) -> impl Iterator<Item = IndexItem<'_, Trait, Name>> {
        self.traits.iter()
    }

    pub fn behaviors(
        &'_ self,
    ) -> impl Iterator<Item = IndexItem<'_, Behavior, (TypeIdx, TypeIdx)>> {
        self.behaviors.iter()
    }

    // Lookup methods by name and scope
    pub fn get_type_definition_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(TypeDefinitionIdx, &TypeDefinition)> {
        self.type_definitions
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_expression_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(ExpressionIdx, &Expression)> {
        self.expressions
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_pattern_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(PatternIdx, &Pattern)> {
        self.patterns.get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_type_reference_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(TypeIdx, &TypeReference)> {
        self.type_references
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_trait_by_name(&self, name: &Name) -> Option<(TraitIdx, &Trait)> {
        self.traits
            .get_by_scoped_name(name, Scopes::ROOT, &self.scopes)
    }

    // Get methods by index
    pub fn get_type_definition(&self, idx: TypeDefinitionIdx) -> &TypeDefinition {
        self.type_definitions.get(idx)
    }

    pub fn get_expression(&self, idx: ExpressionIdx) -> &Expression {
        self.expressions.get(idx)
    }

    pub fn get_expression_range(&self, idx: ExpressionIdx) -> TextRange {
        self.expressions.get_range(idx)
    }

    pub fn get_pattern(&self, idx: PatternIdx) -> &Pattern {
        self.patterns.get(idx)
    }

    pub fn get_pattern_range(&self, idx: PatternIdx) -> TextRange {
        self.patterns.get_range(idx)
    }

    pub fn get_type_reference(&self, idx: TypeIdx) -> &TypeReference {
        self.type_references.get(idx)
    }

    pub fn get_type_reference_range(&self, idx: TypeIdx) -> TextRange {
        self.type_references.get_range(idx)
    }

    pub fn get_import(&self, idx: ImportIdx) -> &Import {
        self.imports.get(idx)
    }

    pub fn get_trait(&self, idx: TraitIdx) -> &Trait {
        self.traits.get(idx)
    }

    pub fn get_behavior(&self, idx: BehaviorIdx) -> &Behavior {
        self.behaviors.get(idx)
    }

    /// Check if a scope is equal to or descended from another scope
    /// Returns true if `child` is the same as `ancestor` or is nested within it
    fn scope_is_descendant_of(&self, child: ScopeIdx, ancestor: ScopeIdx) -> bool {
        let mut current = child;
        loop {
            if current == ancestor {
                return true;
            }
            let parent = self.scopes.parent_scope(current);
            if current == parent {
                // Reached root without finding ancestor
                return false;
            }
            current = parent;
        }
    }

    /// Find the trait that contains the given scope, if any
    /// This checks if the scope is within a trait's scope hierarchy
    pub fn find_trait_containing_scope(&self, scope: ScopeIdx) -> Option<(TraitIdx, &Trait)> {
        for (trait_idx, trait_def, _range, _name_scope) in self.traits() {
            let trait_scope = trait_def.scope();
            if self.scope_is_descendant_of(scope, trait_scope) {
                return Some((trait_idx, trait_def));
            }
        }
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    ConflictingTraitDefinitionName {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
    ConflictingBehaviorDefinition {
        type_: TypeIdx,
        trait_: TypeIdx,
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LoweringWarning {
    kind: LoweringWarningKind,
    range: TextRange,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    #[allow(dead_code)]
    db: &'db dyn HirDatabase,
    glossary: AstGlossary,
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_references: Index<TypeReference>,
    type_definitions: Index<TypeDefinition>,
    traits: Index<Trait>,
    behaviors: Index<Behavior, (TypeIdx, TypeIdx)>,
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
            traits: Index::new(),
            behaviors: Index::new(),
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
                let warn = LoweringWarningKind::UnusedImport {
                    import: import.clone(),
                };
                warnings.push(LoweringWarning {
                    kind: warn,
                    range: self.imports.get_range(id),
                });
            }
        }

        HirModule {
            imports: self.imports,
            expressions: self.expressions,
            patterns: self.patterns,
            type_references: self.type_references,
            type_definitions: self.type_definitions,
            traits: self.traits,
            behaviors: self.behaviors,
            scopes: self.scopes,
            warnings,
            errors: self.errors,
        }
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

            if let Some((_tid, scope)) = self.traits.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first, scope));
            }
            if let Some((_tid, scope)) = self.type_definitions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first, scope));
            }
            if let Some((_tid, scope)) = self.type_references.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first, scope));
            }
            if let Some(_ast) = self.glossary.get_type_definition_by_name(first) {
                return Some(Path::this_module(rest, first, Scopes::ROOT));
            }
            if let Some((import_id, _)) = self.imports.get_id(&local_name, &self.scopes) {
                self.used_imports.insert(import_id);

                let import = self.imports.get(import_id);
                let fqn = Fqn::new(
                    import.segments().iter().cloned(),
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

            if let Some((_pid, scope)) = self.patterns.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first, scope));
            }
            if let Some((_eid, scope)) = self.expressions.get_id(&local_name, &self.scopes) {
                return Some(Path::this_module(rest, first, scope));
            }
            if let Some(_ast) = self.glossary.get_value_by_name(first) {
                return Some(Path::this_module(rest, first, Scopes::ROOT));
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

    pub(crate) fn add_trait(&mut self, trait_: Trait, element: &SyntaxElement) {
        let res = self.traits.insert_named(
            trait_.name.clone(),
            trait_,
            element.text_range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingTraitDefinitionName {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.text_range());
        }
    }

    pub(crate) fn add_behavior(&mut self, behavior: Behavior, element: &SyntaxElement) {
        let res = self.behaviors.insert_named(
            (behavior.attached_type, behavior.attached_trait),
            behavior.clone(),
            element.text_range(),
            &self.scopes,
        );

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingBehaviorDefinition {
                type_: behavior.attached_type,
                trait_: behavior.attached_trait,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.text_range());
        }
    }

    pub(crate) fn add_import(&mut self, segments: &NonEmpty<Name>, element: &SyntaxElement) {
        let new_import = Import::new(segments);
        let last = segments.last();

        if let Some((existing_id, _)) = self.imports.get_id(last, &self.scopes) {
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

/// Lower a raw source file to HIR.
/// This query is cached by salsa, so repeated calls with the same file
/// will return the cached result unless the file contents have changed.
/// Returns both parse errors and the HIR module.
#[salsa::tracked]
pub fn lower_file<'db>(
    db: &'db dyn HirDatabase,
    module_id: ModuleId,
) -> (HirModule, Vec<alloy_parser::ParseError>) {
    let current_file = db.get_source(module_id);
    let current_file = match current_file {
        SourceFile::Raw(raw) => raw,
        SourceFile::Virtual(_) => return (HirModule::empty(), vec![]),
    };

    let (source_file, parse_errors) = ast::source_file(current_file.contents(db));

    // If parsing failed, return an empty HIR module with parse errors
    let Some(source_file) = source_file else {
        return (HirModule::empty(), parse_errors);
    };

    let hir_module = lower_source_file(db, &source_file);
    (hir_module, parse_errors)
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
