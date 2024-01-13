use super::{Fqn, Name};
use crate::index::Index;

use alloy_ast as ast;
use alloy_scope::Scopes;
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

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct HirModule {
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    types: Index<Type>,
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
    ConflictingTypeName {
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
    types: Index<Type>,
    scopes: Scopes,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl LoweringCtx {
    fn new() -> Self {
        Self {
            imports: Index::new(),
            expressions: Index::new(),
            patterns: Index::new(),
            types: Index::new(),
            scopes: Scopes::default(),
            warnings: vec![],
            errors: vec![],
        }
    }

    fn finish(self) -> HirModule {
        HirModule {
            imports: self.imports,
            expressions: self.expressions,
            patterns: self.patterns,
            types: self.types,
            scopes: self.scopes,
            warnings: self.warnings,
            errors: self.errors,
        }
    }

    pub(crate) fn contains_variable_ref(&self, path: &Path) -> bool {
        match path {
            Path::ThisModule(name) => {
                self.expressions.get_id(name, &self.scopes).is_some()
                    || self.patterns.get_id(name, &self.scopes).is_some()
                    || self.imports.get_id(name, &self.scopes).is_some()
            }
            Path::OtherModule(fqn) => self
                .imports
                .get_id(fqn.first_module_segment(), &self.scopes)
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
        let res =
            self.expressions
                .insert_named(name, expression, element.text_range(), &self.scopes);

        if let Err(err) = res {
            let err = LoweringErrorKind::ConflictingValue {
                name: err.name,
                first: err.first,
                second: err.second,
            };
            self.error(err, element.text_range());
        }
    }

    pub(crate) fn add_pattern(&mut self, pattern: Pattern, element: &SyntaxElement) -> PatternIdx {
        match &pattern {
            Pattern::VariableDeclaration { name } => {
                let res = self.patterns.insert_named(
                    name.local_name().clone(),
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

    pub(crate) fn add_type_(&mut self, type_: Type, element: &SyntaxElement) -> TypeIdx {
        self.types.insert_not_named(type_, element.text_range())
    }

    pub(crate) fn add_missing_type(&mut self, element: &SyntaxElement) -> TypeIdx {
        self.add_type_(Type::Missing, element)
    }

    pub(crate) fn add_type_annotation(&mut self, name: Name, type_: &ast::Type) {
        let type_id = lower_type(self, type_);

        let res = self
            .types
            .add_name(name, type_id, type_.range(), &self.scopes);

        match res {
            Err(err) => {
                let err = LoweringErrorKind::ConflictingTypeName {
                    name: err.name,
                    first: err.first,
                    second: err.second,
                };
                self.error(err, type_.range());

                self.add_missing_type(&type_.syntax())
            }
            Ok(pid) => pid,
        };
    }

    pub(crate) fn add_import(&mut self, path: &NonEmpty<Name>, element: &SyntaxElement) {
        let new_import = Import::new(path);
        let name = path.last();

        if let Some(existing_id) = self.imports.get_id(name, &self.scopes) {
            let existing_import = self.imports.get(existing_id);
            let existing_import_range = self.imports.get_range(existing_id);

            if &new_import == existing_import {
                let warn = LoweringWarningKind::DuplicateImport {
                    name: name.clone(),
                    first: existing_import_range,
                    second: element.text_range(),
                };
                self.warning(warn, element.text_range());

                return;
            }
        }

        let res = self.imports.insert_named(
            name.clone(),
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

impl LoweringCtx {
    fn inside_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.scopes.push_scope();

        let res = f(self);

        self.scopes.pop_scope();

        res
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
    fn warning(&mut self, kind: LoweringWarningKind, range: TextRange) {
        self.warnings.push(LoweringWarning { kind, range });
    }

    fn error(&mut self, kind: LoweringErrorKind, range: TextRange) {
        self.errors.push(LoweringError { kind, range });
    }
}
