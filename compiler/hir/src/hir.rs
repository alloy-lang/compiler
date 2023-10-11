use super::{Fqn, Name};
use alloy_ast as ast;
use alloy_ast::AstElement;
use alloy_syntax::SyntaxElement;
use la_arena::Idx;
use la_arena::{Arena, ArenaMap};
use non_empty_vec::NonEmpty;
use ordered_float::NotNan;
use rustc_hash::{FxHashMap, FxHashSet};
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
    imports: FxHashSet<Import>,
    values: Arena<Value>,
    expressions: Arena<Expression>,
    expression_ranges: ArenaMap<ExpressionIdx, TextRange>,
    patterns: Arena<Pattern>,
    pattern_ranges: ArenaMap<PatternIdx, TextRange>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    types: Arena<Type>,
    type_ranges: ArenaMap<TypeIdx, TextRange>,
}

#[derive(Debug)]
pub struct LoweringError(String);

#[derive(Debug)]
struct LoweringCtx {
    imports: FxHashSet<Import>,
    values: Arena<Value>,
    expressions: Arena<Expression>,
    expression_ranges: ArenaMap<ExpressionIdx, TextRange>,
    patterns: Arena<Pattern>,
    pattern_ranges: ArenaMap<PatternIdx, TextRange>,
    type_annotations: FxHashMap<Name, TypeIdx>,
    types: Arena<Type>,
    type_ranges: ArenaMap<TypeIdx, TextRange>,
    errors: Vec<LoweringError>,
}

impl LoweringCtx {
    fn new() -> Self {
        Self {
            imports: FxHashSet::default(),
            values: Arena::default(),
            expressions: Arena::default(),
            expression_ranges: ArenaMap::default(),
            patterns: Arena::default(),
            pattern_ranges: ArenaMap::default(),
            type_annotations: FxHashMap::default(),
            types: Arena::default(),
            type_ranges: ArenaMap::default(),
            errors: vec![],
        }
    }

    fn finish(self) -> (HirModule, Vec<LoweringError>) {
        let hir = HirModule {
            imports: self.imports,
            values: self.values,
            expressions: self.expressions,
            expression_ranges: self.expression_ranges,
            patterns: self.patterns,
            pattern_ranges: self.pattern_ranges,
            type_annotations: self.type_annotations,
            types: self.types,
            type_ranges: self.type_ranges,
        };
        (hir, self.errors)
    }

    pub(crate) fn add_expression(
        &mut self,
        expression: Expression,
        element: &SyntaxElement,
    ) -> ExpressionIdx {
        let idx = self.expressions.alloc(expression);
        self.expression_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_missing_expression(&mut self, element: &SyntaxElement) -> ExpressionIdx {
        let idx = self.expressions.alloc(Expression::Missing);
        self.expression_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_pattern(&mut self, pattern: Pattern, element: &SyntaxElement) -> PatternIdx {
        let idx = self.patterns.alloc(pattern);
        self.pattern_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_missing_pattern(&mut self, element: &SyntaxElement) -> PatternIdx {
        let idx = self.patterns.alloc(Pattern::Missing);
        self.pattern_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_type_(&mut self, type_: Type, element: &SyntaxElement) -> TypeIdx {
        let idx = self.types.alloc(type_);
        self.type_ranges.insert(idx, element.text_range());

        idx
    }

    pub(crate) fn add_type_annotation(&mut self, name: Name, type_id: TypeIdx) {
        self.type_annotations.insert(name, type_id);
    }

    pub(crate) fn add_value(&mut self, value: Value) {
        self.values.alloc(value);
    }

    pub(crate) fn add_import(&mut self, path: NonEmpty<Name>) {
        if !self.imports.insert(Import::new(path)) {
            // todo!("validation")
        }
    }
}

#[must_use]
pub fn lower_source_file(source_file: &ast::SourceFile) -> (HirModule, Vec<LoweringError>) {
    let mut ctx = LoweringCtx::new();
    source_file::lower_source_file(&mut ctx, source_file);
    ctx.finish()
}

#[must_use]
pub fn lower_repl_line(source_file: &ast::SourceFile) -> (HirModule, Vec<LoweringError>) {
    let mut ctx = LoweringCtx::new();
    source_file::lower_repl_line(&mut ctx, source_file);
    ctx.finish()
}

impl LoweringCtx {
    fn error(&mut self, msg: impl Into<String>) {
        self.errors.push(LoweringError(msg.into()));
    }
}
