//! AST walking primitives for tooling (formatters, linters, analyzers).
//!
//! This crate intentionally uses a trait-based visitor as the core API and
//! augments ergonomics with small macros:
//! - [`ast_match!`] for concise matching on AST union nodes
//! - [`visit_each!`] for concise child traversal

pub use alloy_ast as ast;

/// Trait-based AST walker.
///
/// Override only the methods you care about. By default, every `visit_*`
/// method delegates to the corresponding `walk_*` free function, which
/// recursively traverses child nodes.
#[allow(clippy::module_name_repetitions)]
pub trait Walker {
    fn visit_source_file(&mut self, node: &ast::SourceFile) {
        walk_source_file(self, node);
    }

    fn visit_statement(&mut self, node: &ast::Statement) {
        walk_statement(self, node);
    }

    fn visit_module_def(&mut self, node: &ast::ModuleDef) {
        walk_module_def(self, node);
    }

    fn visit_import_def(&mut self, node: &ast::ImportDef) {
        walk_import_def(self, node);
    }

    fn visit_import_def_child(&mut self, node: &ast::ImportDefChild) {
        walk_import_def_child(self, node);
    }

    fn visit_import_def_segment(&mut self, node: &ast::ImportDefSegment) {
        walk_import_def_segment(self, node);
    }

    fn visit_import_def_group(&mut self, node: &ast::ImportDefGroup) {
        walk_import_def_group(self, node);
    }

    fn visit_trait_def(&mut self, node: &ast::TraitDef) {
        walk_trait_def(self, node);
    }

    fn visit_behavior_def(&mut self, node: &ast::BehaviorDef) {
        walk_behavior_def(self, node);
    }

    fn visit_type_definition(&mut self, node: &ast::TypeDefinition) {
        walk_type_definition(self, node);
    }

    fn visit_type_definition_member(&mut self, node: &ast::TypeDefinitionMember) {
        walk_type_definition_member(self, node);
    }

    fn visit_type_annotation(&mut self, node: &ast::TypeAnnotation) {
        walk_type_annotation(self, node);
    }

    fn visit_value_def(&mut self, node: &ast::ValueDef) {
        walk_value_def(self, node);
    }

    fn visit_named_type_variable(&mut self, node: &ast::NamedTypeVariable) {
        walk_named_type_variable(self, node);
    }

    fn visit_self_type_variable(&mut self, node: &ast::SelfTypeVariable) {
        walk_self_type_variable(self, node);
    }

    fn visit_type_variable_constraint(&mut self, node: &ast::TypeVariableConstraint) {
        walk_type_variable_constraint(self, node);
    }

    fn visit_type_variable_kind_constraint(&mut self, node: &ast::TypeVariableKindConstraint) {
        walk_type_variable_kind_constraint(self, node);
    }

    fn visit_type_variable_trait_constraint(&mut self, node: &ast::TypeVariableTraitConstraint) {
        walk_type_variable_trait_constraint(self, node);
    }

    fn visit_expression(&mut self, node: &ast::Expression) {
        walk_expression(self, node);
    }

    fn visit_infix_expr(&mut self, node: &ast::InfixExpr) {
        walk_infix_expr(self, node);
    }

    fn visit_variable_ref(&mut self, node: &ast::VariableRef) {
        walk_variable_ref(self, node);
    }

    fn visit_if_then_else_expr(&mut self, node: &ast::IfThenElseExpr) {
        walk_if_then_else_expr(self, node);
    }

    fn visit_paren_expr(&mut self, node: &ast::ParenExpr) {
        walk_paren_expr(self, node);
    }

    fn visit_tuple_expr(&mut self, node: &ast::TupleExpr) {
        walk_tuple_expr(self, node);
    }

    fn visit_unary_expr(&mut self, node: &ast::UnaryExpr) {
        walk_unary_expr(self, node);
    }

    fn visit_lambda_expr(&mut self, node: &ast::LambdaExpr) {
        walk_lambda_expr(self, node);
    }

    fn visit_lambda_expr_arg(&mut self, node: &ast::LambdaExprArg) {
        walk_lambda_expr_arg(self, node);
    }

    fn visit_function_call(&mut self, node: &ast::FunctionCall) {
        walk_function_call(self, node);
    }

    fn visit_match_expr(&mut self, node: &ast::MatchExpr) {
        walk_match_expr(self, node);
    }

    fn visit_match_target(&mut self, node: &ast::MatchTarget) {
        walk_match_target(self, node);
    }

    fn visit_pattern(&mut self, node: &ast::Pattern) {
        walk_pattern(self, node);
    }

    fn visit_destructure(&mut self, node: &ast::Destructure) {
        walk_destructure(self, node);
    }

    fn visit_paren_pattern(&mut self, node: &ast::ParenPattern) {
        walk_paren_pattern(self, node);
    }

    fn visit_tuple_pattern(&mut self, node: &ast::TuplePattern) {
        walk_tuple_pattern(self, node);
    }

    fn visit_tuple_pattern_arg(&mut self, node: &ast::TuplePatternArg) {
        walk_tuple_pattern_arg(self, node);
    }

    fn visit_type(&mut self, node: &ast::Type) {
        walk_type(self, node);
    }

    fn visit_type_identifier(&mut self, node: &ast::TypeIdentifier) {
        walk_type_identifier(self, node);
    }

    fn visit_lambda_type(&mut self, node: &ast::LambdaType) {
        walk_lambda_type(self, node);
    }

    fn visit_tuple_type(&mut self, node: &ast::TupleType) {
        walk_tuple_type(self, node);
    }

    fn visit_parenthesized_type(&mut self, node: &ast::ParenthesizedType) {
        walk_parenthesized_type(self, node);
    }

    fn visit_bounded_type(&mut self, node: &ast::BoundedType) {
        walk_bounded_type(self, node);
    }

    fn visit_path(&mut self, node: &ast::Path) {
        walk_path(self, node);
    }

    fn visit_int_literal(&mut self, node: &ast::IntLiteral) {
        walk_int_literal(self, node);
    }

    fn visit_fraction_literal(&mut self, node: &ast::FractionLiteral) {
        walk_fraction_literal(self, node);
    }

    fn visit_string_literal(&mut self, node: &ast::StringLiteral) {
        walk_string_literal(self, node);
    }

    fn visit_char_literal(&mut self, node: &ast::CharLiteral) {
        walk_char_literal(self, node);
    }

    fn visit_unit(&mut self, node: &ast::Unit) {
        walk_unit(self, node);
    }

    fn visit_nil_identifier(&mut self, node: &ast::NilIdentifier) {
        walk_nil_identifier(self, node);
    }

    fn visit_variable_declaration(&mut self, node: &ast::VariableDeclaration) {
        walk_variable_declaration(self, node);
    }

    fn visit_self_type(&mut self, node: &ast::SelfType) {
        walk_self_type(self, node);
    }

    fn visit_unit_type(&mut self, node: &ast::UnitType) {
        walk_unit_type(self, node);
    }
}

pub fn walk_source_file<W: Walker + ?Sized>(walker: &mut W, node: &ast::SourceFile) {
    for statement in node.statements() {
        walker.visit_statement(&statement);
    }
}

pub fn walk_statement<W: Walker + ?Sized>(walker: &mut W, node: &ast::Statement) {
    ast_match!(node, Statement {
        ModuleDef(module_def) => { walker.visit_module_def(module_def); },
        ImportDef(import_def) => { walker.visit_import_def(import_def); },
        TraitDef(trait_def) => { walker.visit_trait_def(trait_def); },
        BehaviorDef(behavior_def) => { walker.visit_behavior_def(behavior_def); },
        TypeDefinition(type_definition) => { walker.visit_type_definition(type_definition); },
        TypeAnnotation(type_annotation) => { walker.visit_type_annotation(type_annotation); },
        ValueDef(value_def) => { walker.visit_value_def(value_def); },
        Expression(expression) => { walker.visit_expression(expression); },
    });
}

pub fn walk_module_def<W: Walker + ?Sized>(walker: &mut W, node: &ast::ModuleDef) {
    visit_each!(walker, node, {
        imports => visit_import_def,
        traits => visit_trait_def,
        behaviors => visit_behavior_def,
        type_definitions => visit_type_definition,
        type_annotations => visit_type_annotation,
        values => visit_value_def,
    });
}

pub fn walk_import_def<W: Walker + ?Sized>(walker: &mut W, node: &ast::ImportDef) {
    visit_each!(walker, node, {
        children => visit_import_def_child,
    });
}

pub fn walk_import_def_child<W: Walker + ?Sized>(walker: &mut W, node: &ast::ImportDefChild) {
    ast_match!(node, ImportDefChild {
        ImportDefSegment(segment) => { walker.visit_import_def_segment(segment); },
        ImportDefGroup(group) => { walker.visit_import_def_group(group); },
    });
}

pub fn walk_import_def_segment<W: Walker + ?Sized>(_: &mut W, _: &ast::ImportDefSegment) {}

pub fn walk_import_def_group<W: Walker + ?Sized>(walker: &mut W, node: &ast::ImportDefGroup) {
    visit_each!(walker, node, {
        children => visit_import_def_segment,
    });
}

pub fn walk_trait_def<W: Walker + ?Sized>(walker: &mut W, node: &ast::TraitDef) {
    visit_each!(walker, node, {
        self_type_variables => visit_self_type_variable,
        named_type_variables => visit_named_type_variable,
        type_annotations => visit_type_annotation,
        values => visit_value_def,
    });
}

pub fn walk_behavior_def<W: Walker + ?Sized>(walker: &mut W, node: &ast::BehaviorDef) {
    visit_each!(walker, node, {
        ?trait_ => visit_type,
        ?type_ => visit_type,
        named_type_variables => visit_named_type_variable,
        type_annotations => visit_type_annotation,
        values => visit_value_def,
    });
}

pub fn walk_type_definition<W: Walker + ?Sized>(walker: &mut W, node: &ast::TypeDefinition) {
    visit_each!(walker, node, {
        types => visit_type_definition_member,
    });
}

pub fn walk_type_definition_member<W: Walker + ?Sized>(
    walker: &mut W,
    node: &ast::TypeDefinitionMember,
) {
    for property in node.properties() {
        walker.visit_type(&property);
    }
}

pub fn walk_type_annotation<W: Walker + ?Sized>(walker: &mut W, node: &ast::TypeAnnotation) {
    visit_each!(walker, node, {
        ?type_ => visit_type,
        named_type_variables => visit_named_type_variable,
    });
}

pub fn walk_value_def<W: Walker + ?Sized>(walker: &mut W, node: &ast::ValueDef) {
    visit_each!(walker, node, {
        ?value => visit_expression,
    });
}

pub fn walk_named_type_variable<W: Walker + ?Sized>(walker: &mut W, node: &ast::NamedTypeVariable) {
    visit_each!(walker, node, {
        constraints => visit_type_variable_constraint,
    });
}

pub fn walk_self_type_variable<W: Walker + ?Sized>(walker: &mut W, node: &ast::SelfTypeVariable) {
    visit_each!(walker, node, {
        constraints => visit_type_variable_constraint,
    });
}

pub fn walk_type_variable_constraint<W: Walker + ?Sized>(
    walker: &mut W,
    node: &ast::TypeVariableConstraint,
) {
    ast_match!(node, TypeVariableConstraint {
        TypeVariableKindConstraint(kind) => { walker.visit_type_variable_kind_constraint(kind); },
        TypeVariableTraitConstraint(trait_constraint) => { walker.visit_type_variable_trait_constraint(trait_constraint); },
    });
}

pub fn walk_type_variable_kind_constraint<W: Walker + ?Sized>(
    _: &mut W,
    _: &ast::TypeVariableKindConstraint,
) {
}

pub fn walk_type_variable_trait_constraint<W: Walker + ?Sized>(
    walker: &mut W,
    node: &ast::TypeVariableTraitConstraint,
) {
    visit_each!(walker, node, {
        ?trait_ => visit_path,
    });
}

pub fn walk_expression<W: Walker + ?Sized>(walker: &mut W, node: &ast::Expression) {
    ast_match!(node, Expression {
        IntLiteral(int_literal) => { walker.visit_int_literal(int_literal); },
        FractionLiteral(fraction_literal) => { walker.visit_fraction_literal(fraction_literal); },
        StringLiteral(string_literal) => { walker.visit_string_literal(string_literal); },
        CharLiteral(char_literal) => { walker.visit_char_literal(char_literal); },
        VariableRef(variable_ref) => { walker.visit_variable_ref(variable_ref); },
        InfixExpr(infix_expr) => { walker.visit_infix_expr(infix_expr); },
        Unit(unit) => { walker.visit_unit(unit); },
        IfThenElseExpr(if_then_else_expr) => { walker.visit_if_then_else_expr(if_then_else_expr); },
        ParenExpr(paren_expr) => { walker.visit_paren_expr(paren_expr); },
        TupleExpr(tuple_expr) => { walker.visit_tuple_expr(tuple_expr); },
        UnaryExpr(unary_expr) => { walker.visit_unary_expr(unary_expr); },
        LambdaExpr(lambda_expr) => { walker.visit_lambda_expr(lambda_expr); },
        FunctionCall(function_call) => { walker.visit_function_call(function_call); },
        MatchExpr(match_expr) => { walker.visit_match_expr(match_expr); },
    });
}

pub fn walk_infix_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::InfixExpr) {
    visit_each!(walker, node, {
        ?lhs => visit_expression,
        ?rhs => visit_expression,
    });
}

pub fn walk_variable_ref<W: Walker + ?Sized>(walker: &mut W, node: &ast::VariableRef) {
    visit_each!(walker, node, {
        ?name => visit_path,
    });
}

pub fn walk_if_then_else_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::IfThenElseExpr) {
    visit_each!(walker, node, {
        ?condition => visit_expression,
        ?then => visit_expression,
        ?else_ => visit_expression,
    });
}

pub fn walk_paren_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::ParenExpr) {
    visit_each!(walker, node, {
        ?expression => visit_expression,
    });
}

pub fn walk_tuple_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::TupleExpr) {
    visit_each!(walker, node, {
        expressions => visit_expression,
    });
}

pub fn walk_unary_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::UnaryExpr) {
    visit_each!(walker, node, {
        ?expression => visit_expression,
    });
}

pub fn walk_lambda_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::LambdaExpr) {
    visit_each!(walker, node, {
        args => visit_lambda_expr_arg,
        ?body => visit_expression,
    });
}

pub fn walk_lambda_expr_arg<W: Walker + ?Sized>(walker: &mut W, node: &ast::LambdaExprArg) {
    visit_each!(walker, node, {
        ?pattern => visit_pattern,
    });
}

pub fn walk_function_call<W: Walker + ?Sized>(walker: &mut W, node: &ast::FunctionCall) {
    visit_each!(walker, node, {
        ?target => visit_variable_ref,
        args => visit_expression,
    });
}

pub fn walk_match_expr<W: Walker + ?Sized>(walker: &mut W, node: &ast::MatchExpr) {
    visit_each!(walker, node, {
        ?condition => visit_expression,
        targets => visit_match_target,
    });
}

pub fn walk_match_target<W: Walker + ?Sized>(walker: &mut W, node: &ast::MatchTarget) {
    visit_each!(walker, node, {
        ?condition => visit_pattern,
        ?value => visit_expression,
    });
}

pub fn walk_pattern<W: Walker + ?Sized>(walker: &mut W, node: &ast::Pattern) {
    ast_match!(node, Pattern {
        IntLiteral(int_literal) => { walker.visit_int_literal(int_literal); },
        FractionLiteral(fraction_literal) => { walker.visit_fraction_literal(fraction_literal); },
        StringLiteral(string_literal) => { walker.visit_string_literal(string_literal); },
        CharLiteral(char_literal) => { walker.visit_char_literal(char_literal); },
        VariableDeclaration(variable_declaration) => { walker.visit_variable_declaration(variable_declaration); },
        NilIdentifier(nil_identifier) => { walker.visit_nil_identifier(nil_identifier); },
        Destructure(destructure) => { walker.visit_destructure(destructure); },
        Unit(unit) => { walker.visit_unit(unit); },
        ParenPattern(paren_pattern) => { walker.visit_paren_pattern(paren_pattern); },
        TuplePattern(tuple_pattern) => { walker.visit_tuple_pattern(tuple_pattern); },
    });
}

pub fn walk_destructure<W: Walker + ?Sized>(walker: &mut W, node: &ast::Destructure) {
    visit_each!(walker, node, {
        ?target => visit_path,
        args => visit_pattern,
    });
}

pub fn walk_paren_pattern<W: Walker + ?Sized>(walker: &mut W, node: &ast::ParenPattern) {
    visit_each!(walker, node, {
        ?pattern => visit_tuple_pattern_arg,
    });
}

pub fn walk_tuple_pattern<W: Walker + ?Sized>(walker: &mut W, node: &ast::TuplePattern) {
    visit_each!(walker, node, {
        patterns => visit_tuple_pattern_arg,
    });
}

pub fn walk_tuple_pattern_arg<W: Walker + ?Sized>(walker: &mut W, node: &ast::TuplePatternArg) {
    visit_each!(walker, node, {
        ?arg => visit_pattern,
    });
}

pub fn walk_type<W: Walker + ?Sized>(walker: &mut W, node: &ast::Type) {
    ast_match!(node, Type {
        SelfType(self_type) => { walker.visit_self_type(self_type); },
        UnitType(unit_type) => { walker.visit_unit_type(unit_type); },
        NilIdentifier(nil_identifier) => { walker.visit_nil_identifier(nil_identifier); },
        TypeIdentifier(type_identifier) => { walker.visit_type_identifier(type_identifier); },
        LambdaType(lambda_type) => { walker.visit_lambda_type(lambda_type); },
        TupleType(tuple_type) => { walker.visit_tuple_type(tuple_type); },
        ParenthesizedType(parenthesized_type) => { walker.visit_parenthesized_type(parenthesized_type); },
        BoundedType(bounded_type) => { walker.visit_bounded_type(bounded_type); },
    });
}

pub fn walk_type_identifier<W: Walker + ?Sized>(walker: &mut W, node: &ast::TypeIdentifier) {
    visit_each!(walker, node, {
        ?name => visit_path,
    });
}

pub fn walk_lambda_type<W: Walker + ?Sized>(walker: &mut W, node: &ast::LambdaType) {
    visit_each!(walker, node, {
        ?arg_type => visit_type,
        ?return_type => visit_type,
    });
}

pub fn walk_tuple_type<W: Walker + ?Sized>(walker: &mut W, node: &ast::TupleType) {
    visit_each!(walker, node, {
        members => visit_type,
    });
}

pub fn walk_parenthesized_type<W: Walker + ?Sized>(walker: &mut W, node: &ast::ParenthesizedType) {
    visit_each!(walker, node, {
        ?inner => visit_type,
    });
}

pub fn walk_bounded_type<W: Walker + ?Sized>(walker: &mut W, node: &ast::BoundedType) {
    visit_each!(walker, node, {
        ?base => visit_type,
        args => visit_type,
    });
}

pub fn walk_path<W: Walker + ?Sized>(_: &mut W, _: &ast::Path) {}
pub fn walk_int_literal<W: Walker + ?Sized>(_: &mut W, _: &ast::IntLiteral) {}
pub fn walk_fraction_literal<W: Walker + ?Sized>(_: &mut W, _: &ast::FractionLiteral) {}
pub fn walk_string_literal<W: Walker + ?Sized>(_: &mut W, _: &ast::StringLiteral) {}
pub fn walk_char_literal<W: Walker + ?Sized>(_: &mut W, _: &ast::CharLiteral) {}
pub fn walk_unit<W: Walker + ?Sized>(_: &mut W, _: &ast::Unit) {}
pub fn walk_nil_identifier<W: Walker + ?Sized>(_: &mut W, _: &ast::NilIdentifier) {}
pub fn walk_variable_declaration<W: Walker + ?Sized>(_: &mut W, _: &ast::VariableDeclaration) {}
pub fn walk_self_type<W: Walker + ?Sized>(_: &mut W, _: &ast::SelfType) {}
pub fn walk_unit_type<W: Walker + ?Sized>(_: &mut W, _: &ast::UnitType) {}

/// Match helper for AST union nodes.
///
/// # Example
///
/// ```ignore
/// ast_match!(stmt, Statement {
///     ValueDef(value) => { self.visit_value_def(value); },
///     _ => { ast_walk::walk_statement(self, stmt); },
/// });
/// ```
#[macro_export]
macro_rules! ast_match {
    (
        $value:expr,
        $enum_ty:ident {
            $( $variant:ident($binding:pat) => $body:block, )+
            _ => $fallback:block $(,)?
        }
    ) => {
        match $value {
            $( $crate::ast::$enum_ty::$variant($binding) => $body, )+
            _ => $fallback,
        }
    };
    (
        $value:expr,
        $enum_ty:ident {
            $( $variant:ident($binding:pat) => $body:block ),+ $(,)?
        }
    ) => {
        match $value {
            $( $crate::ast::$enum_ty::$variant($binding) => $body, )+
        }
    };
}

/// Child traversal helper.
///
/// List children use `getter => visit_method`.
/// Optional children use `?getter => visit_method`.
///
/// # Example
///
/// ```ignore
/// visit_each!(self, node, {
///     ?condition => visit_expression,
///     targets => visit_match_target,
/// });
/// ```
#[macro_export]
macro_rules! visit_each {
    ($walker:expr, $node:expr, { $($items:tt)* }) => {
        $crate::visit_each!(@items $walker, $node, $($items)*);
    };

    (@items $walker:expr, $node:expr,) => {};

    (@items $walker:expr, $node:expr, ?$getter:ident => $visit:ident, $($rest:tt)*) => {
        if let Some(child) = $node.$getter() {
            $walker.$visit(&child);
        }
        $crate::visit_each!(@items $walker, $node, $($rest)*);
    };

    (@items $walker:expr, $node:expr, $getter:ident => $visit:ident, $($rest:tt)*) => {
        for child in $node.$getter() {
            $walker.$visit(&child);
        }
        $crate::visit_each!(@items $walker, $node, $($rest)*);
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstElement;

    fn parse_source_file(input: &str) -> ast::SourceFile {
        let parse_tree = alloy_parser::parse_source_file(input);
        ast::SourceFile::cast(parse_tree.syntax()).expect("expected SourceFile")
    }

    #[derive(Default)]
    struct Counter {
        value_defs: usize,
        lambda_exprs: usize,
    }

    impl Walker for Counter {
        fn visit_value_def(&mut self, node: &ast::ValueDef) {
            self.value_defs += 1;
            walk_value_def(self, node);
        }

        fn visit_lambda_expr(&mut self, node: &ast::LambdaExpr) {
            self.lambda_exprs += 1;
            walk_lambda_expr(self, node);
        }
    }

    #[test]
    fn walker_counts_expected_nodes() {
        let source = parse_source_file("let id = |x| -> x\nlet a = 1");

        let mut counter = Counter::default();
        walk_source_file(&mut counter, &source);

        assert_eq!(counter.value_defs, 2);
        assert_eq!(counter.lambda_exprs, 1);
    }

    #[derive(Default)]
    struct StatementCounter {
        values: usize,
        expressions: usize,
    }

    impl Walker for StatementCounter {
        fn visit_statement(&mut self, stmt: &ast::Statement) {
            ast_match!(stmt, Statement {
                ValueDef(value) => {
                    self.values += 1;
                    self.visit_value_def(value);
                },
                Expression(expression) => {
                    self.expressions += 1;
                    self.visit_expression(expression);
                },
                _ => {
                    walk_statement(self, stmt);
                },
            });
        }
    }

    #[test]
    fn ast_match_macro_supports_partial_dispatch() {
        let source = parse_source_file("let a = 1\n1");

        let mut counter = StatementCounter::default();
        walk_source_file(&mut counter, &source);

        assert_eq!(counter.values, 1);
        assert_eq!(counter.expressions, 1);
    }
}
