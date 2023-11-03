use super::{Fqn, Name};
use crate::index::Index;

use alloy_ast as ast;
use alloy_syntax::SyntaxElement;
use ast::AstElement;
use la_arena::Idx;
use la_arena::{Arena, ArenaMap};
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;
use rustc_hash::FxHashMap;
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

mod r#type;
pub use r#type::*;

mod type_annotation;
pub use type_annotation::*;

mod type_variable;
pub use type_variable::*;

mod type_definition;
pub use type_definition::*;

mod value;
pub use value::*;

#[derive(Debug)]
pub struct HirModule {
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    types: Arena<Type>,
    type_ranges: ArenaMap<TypeIdx, TextRange>,
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
}

#[derive(Debug)]
pub struct LoweringError {
    kind: LoweringErrorKind,
    range: TextRange,
}

#[derive(Debug)]
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
    UnknownReference {
        path: Path,
    },
}

#[derive(Debug)]
pub struct LoweringWarning {
    kind: LoweringWarningKind,
    range: TextRange,
}

#[derive(Debug)]
pub enum LoweringWarningKind {
    DuplicateImport {
        name: Name,
        first: TextRange,
        second: TextRange,
    },
}

#[derive(Debug)]
struct LoweringCtx {
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    types: Arena<Type>,
    type_ranges: ArenaMap<TypeIdx, TextRange>,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl LoweringCtx {
    fn new() -> Self {
        // TODO: Keep track of Scope for variables
        Self {
            imports: Index::new(),
            expressions: Index::new(),
            patterns: Index::new(),
            type_annotations: FxHashMap::default(),
            types: Arena::default(),
            type_ranges: ArenaMap::default(),
            warnings: vec![],
            errors: vec![],
        }
    }

    fn finish(self) -> HirModule {
        HirModule {
            imports: self.imports,
            expressions: self.expressions,
            patterns: self.patterns,
            type_annotations: self.type_annotations,
            types: self.types,
            type_ranges: self.type_ranges,
            warnings: self.warnings,
            errors: self.errors,
        }
    }

    pub(crate) fn contains_variable_ref(&self, path: &Path) -> bool {
        match path {
            Path::ThisModule(name) => {
                self.expressions.get_id(name).is_some() || self.patterns.get_id(name).is_some()
            },
            Path::OtherModule(fqn) => self
                .imports
                .get_by_name(fqn.first_module_segment())
                .is_some(),
        }
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
    ) {
        let res = self
            .expressions
            .insert_named(name, expression, element.text_range());

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingValue {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, &element.text_range());
        }
    }

    pub(crate) fn add_pattern(&mut self, pattern: Pattern, element: &SyntaxElement) -> PatternIdx {
        self.patterns
            .insert_not_named(pattern, element.text_range())
    }

    pub(crate) fn add_missing_pattern(&mut self, element: &SyntaxElement) -> PatternIdx {
        self.add_pattern(Pattern::Missing, element)
    }

    pub(crate) fn add_type_(&mut self, type_: Type, element: &SyntaxElement) -> TypeIdx {
        let idx = self.types.alloc(type_);
        self.type_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_type_annotation(&mut self, name: Name, type_id: TypeIdx) {
        self.type_annotations.insert(name, type_id);
    }

    pub(crate) fn add_import(&mut self, path: NonEmpty<Name>, element: SyntaxElement) {
        let name = path.last().clone();
        let new_import = Import::new(path);

        if let Some(existing_id) = self.imports.get_id(&name) {
            let existing_import = self.imports.get(existing_id);
            let existing_import_range = self.imports.get_range(existing_id);

            if &new_import == existing_import {
                let warn = LoweringWarningKind::DuplicateImport {
                    name,
                    first: existing_import_range,
                    second: element.text_range(),
                };
                self.warning(warn, &element.text_range());

                return;
            }
        }

        let res = self
            .imports
            .insert_named(name, new_import.clone(), element.text_range());

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingImport {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, &element.text_range());
        }
    }
}

#[must_use]
pub fn lower_source_file(source_file: &ast::SourceFile) -> HirModule {
    let mut ctx = LoweringCtx::new();
    source_file::lower_source_file(&mut ctx, source_file);
    ctx.finish()
}

#[must_use]
pub fn lower_repl_line(source_file: &ast::SourceFile) -> HirModule {
    let mut ctx = LoweringCtx::new();
    source_file::lower_repl_line(&mut ctx, source_file);
    ctx.finish()
}

impl LoweringCtx {
    fn warning(&mut self, kind: LoweringWarningKind, range: &TextRange) {
        self.warnings.push(LoweringWarning {
            kind,
            range: *range,
        });
    }

    fn error(&mut self, kind: LoweringErrorKind, range: &TextRange) {
        self.errors.push(LoweringError {
            kind,
            range: *range,
        });
    }
}
