use itertools::Itertools;
use linked_hash_map::LinkedHashMap;

use crate::parse;
use crate::parse::{Expr, LiteralData, Pattern};
use crate::types::Type;

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct TypeEnvironment {
    expr_types: LinkedHashMap<Expr, Type>,
    pattern_types: LinkedHashMap<parse::Pattern, Type>,
}

impl TypeEnvironment {
    pub(crate) fn new() -> Self {
        TypeEnvironment {
            expr_types: LinkedHashMap::new(),
            pattern_types: LinkedHashMap::new(),
        }
    }

    fn must_get_expr_type(&self, expr: &Expr) -> &Type {
        self.get_expr_type(expr)
            .unwrap_or_else(|| panic!("No type found for expression '{:?}'", expr))
    }

    fn get_expr_type(&self, expr: &Expr) -> Option<&Type> {
        self.expr_types.get(expr)
    }

    fn must_get_pattern_type(&self, pattern: &parse::Pattern) -> &Type {
        self.get_pattern_type(pattern)
            .unwrap_or_else(|| panic!("No type found for pattern '{:?}'", pattern))
    }

    fn get_pattern_type(&self, pattern: &parse::Pattern) -> Option<&Type> {
        self.pattern_types.get(pattern)
    }

    fn insert_expr_type(&mut self, expr: &Expr, t: Type) {
        self.expr_types.entry(expr.clone()).or_insert(t);
    }

    fn insert_pattern_type(&mut self, pattern: &parse::Pattern, t: Type) {
        self.pattern_types.entry(pattern.clone()).or_insert(t);
    }

    fn insert_literal_data_type(&mut self, data: &LiteralData) {
        let t = match data {
            LiteralData::Integral(_) => Type::identifier("Int"),
            LiteralData::Fractional(_) => Type::identifier("Float"),
            LiteralData::String(_) => Type::identifier("String"),
            LiteralData::Char(_) => Type::identifier("Char"),
        };

        self.insert_pattern_type(&Pattern::Literal(data.clone()), t.clone());
        self.insert_expr_type(&Expr::Literal(data.clone()), t);
    }

    pub(crate) fn insert_expr_type_by_name(&mut self, name: &str, t: Type) {
        self.expr_types.entry(Expr::identifier(name)).or_insert(t);
    }

    fn insert_identifier(&mut self, name: &str) {
        let type_id = format!("T{}", self.expr_types.len());

        self.expr_types
            .entry(Expr::identifier(name))
            .or_insert(Type::variable(type_id.clone()));
        self.pattern_types
            .entry(Pattern::identifier(name))
            .or_insert(Type::variable(type_id));
    }

    fn insert_expr_type_variable(&mut self, expr: &Expr) {
        let type_id = format!("T{}", self.expr_types.len());

        self.expr_types
            .entry(expr.clone())
            .or_insert(Type::variable(type_id));
    }
}

pub(crate) fn infer_type(what_are_you: &Expr, context_type_map: &TypeEnvironment) -> Type {
    let type_map = {
        let mut type_map = context_type_map.clone();
        assign_expr_type_names(what_are_you, &mut type_map);
        // println!("type_map: {:?}\n\n", type_map);

        type_map
    };

    let type_equations = {
        let mut type_equations = Vec::new();
        generate_type_equations(what_are_you, &type_map, &mut type_equations);
        // println!("type_equations: {:?}\n\n", type_equations);

        type_equations
    };

    let substitutions = {
        let mut substitutions = LinkedHashMap::new();

        type_equations
            .into_iter()
            .find(|(left, right, _original_expr)| {
                unify(left.clone(), right.clone(), &mut substitutions).is_none()
            });
        // println!("\n\n\nsubstitutions: {:?}\n\n", substitutions);

        substitutions
    };

    apply_unifier(type_map.must_get_expr_type(what_are_you), &substitutions)
}

fn assign_expr_type_names<'a>(what_are_you: &'a Expr, type_map: &'a mut TypeEnvironment) {
    match what_are_you {
        Expr::Literal(data) => {
            type_map.insert_literal_data_type(data);
        }
        Expr::Identifier(id) => {
            type_map.insert_identifier(id);
        }
        Expr::Lambda(arg, expr) => {
            type_map.insert_expr_type_variable(what_are_you);

            assign_pattern_type_names(arg, type_map);
            assign_expr_type_names(expr, type_map);
        }
        Expr::OpApply(left, _op, right) => {
            type_map.insert_expr_type_variable(what_are_you);

            assign_expr_type_names(left, type_map);
            assign_expr_type_names(right, type_map);
        }
        Expr::IfElse(condition, if_expr, else_expr) => {
            type_map.insert_expr_type_variable(what_are_you);

            assign_expr_type_names(condition, type_map);
            assign_expr_type_names(if_expr, type_map);
            assign_expr_type_names(else_expr, type_map);
        }
        Expr::Apply(func, arg) => {
            type_map.insert_expr_type_variable(what_are_you);

            assign_expr_type_names(func, type_map);
            assign_expr_type_names(arg, type_map);
        }
        Expr::Case(test_expr, branches) => {
            type_map.insert_expr_type_variable(what_are_you);
            assign_expr_type_names(test_expr, type_map);

            branches.iter().for_each(|alt| {
                assign_pattern_type_names(&alt.pattern, type_map);

                if let Some(match_pattern_expr) = pattern_to_expr(&alt.pattern) {
                    assign_expr_type_names(&match_pattern_expr, type_map)
                }

                let parse::Match::Simple(match_result_expr) = &alt.matches;
                assign_expr_type_names(match_result_expr, type_map)
            });
        }
        Expr::Paren(expr) => {
            type_map.insert_expr_type_variable(what_are_you);

            assign_expr_type_names(expr, type_map);
        }
    };
}

fn assign_pattern_type_names<'a>(
    what_are_you: &'a parse::Pattern,
    type_map: &'a mut TypeEnvironment,
) {
    match what_are_you {
        Pattern::Literal(data) => {
            type_map.insert_literal_data_type(data);
        }
        Pattern::Identifier(id) => {
            type_map.insert_identifier(id);
        }
        Pattern::Tuple(args) => {
            args.iter()
                .for_each(|arg| assign_pattern_type_names(arg, type_map));

            let t = Type::tuple(
                args.iter()
                    .map(|arg| type_map.must_get_pattern_type(arg).clone())
                    .collect(),
            );

            type_map.insert_pattern_type(what_are_you, t.clone());

            if let Some(expr) = pattern_to_expr(what_are_you) {
                type_map.insert_expr_type(&expr, t);
            }
        }
        Pattern::Constructor(_, _) => todo!("Pattern::Constructor(name, args)"),
        // Pattern::Constructor(name, args) => {
        //     args.iter()
        //         .for_each(|arg| assign_pattern_type_names(arg, type_map));
        //
        //     let t = args
        //         .iter()
        //         .map(|arg| type_map.must_get_pattern_type(arg).clone())
        //         .fold(Type::identifier(name), |lhs, rhs| Type::lambda(lhs, rhs));
        //
        //     type_map.insert_pattern_type(what_are_you, t.clone());
        //
        //     if let Some(expr) = pattern_to_expr(what_are_you) {
        //         type_map.insert_expr_type(&expr, t);
        //     }
        // }
        Pattern::WildCard => todo!("Pattern::WildCard"),
    }
}

fn generate_type_equations<'a>(
    what_are_you: &'a Expr,
    type_map: &TypeEnvironment,
    type_equations: &mut Vec<(Type, Type, Expr)>,
) {
    match what_are_you {
        Expr::Literal(data) => match data {
            LiteralData::Integral(_) => type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                Type::identifier("Int"),
                what_are_you.clone(),
            )),
            LiteralData::Fractional(_) => type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                Type::identifier("Float"),
                what_are_you.clone(),
            )),
            LiteralData::String(_) => type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                Type::identifier("String"),
                what_are_you.clone(),
            )),
            LiteralData::Char(_) => type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                Type::identifier("Char"),
                what_are_you.clone(),
            )),
        },
        Expr::Identifier(_) => {
            // intentionally empty
        }
        Expr::Lambda(arg, expr) => {
            generate_type_equations(expr, type_map, type_equations);

            type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                Type::lambda(
                    type_map.must_get_pattern_type(arg).clone(),
                    type_map.must_get_expr_type(expr).clone(),
                ),
                what_are_you.clone(),
            ));
        }
        Expr::OpApply(left, op, right) => {
            generate_type_equations(left, type_map, type_equations);
            generate_type_equations(right, type_map, type_equations);

            match op.as_str() {
                // trait Eq
                "==" | "!=" => {
                    type_equations.push((
                        type_map.must_get_expr_type(left).clone(),
                        Type::identifier("Int"),
                        what_are_you.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(right).clone(),
                        Type::identifier("Int"),
                        *right.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(what_are_you).clone(),
                        Type::identifier("Bool"),
                        what_are_you.clone(),
                    ));
                }
                // trait Ord
                "<" | "<=" | ">" | ">=" => {
                    type_equations.push((
                        type_map.must_get_expr_type(left).clone(),
                        Type::identifier("Int"),
                        what_are_you.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(right).clone(),
                        Type::identifier("Int"),
                        *right.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(what_are_you).clone(),
                        Type::identifier("Bool"),
                        what_are_you.clone(),
                    ));
                }
                // trait Num
                "+" | "-" | "*" | "/" => {
                    type_equations.push((
                        type_map.must_get_expr_type(left).clone(),
                        Type::identifier("Int"),
                        *left.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(right).clone(),
                        Type::identifier("Int"),
                        *right.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get_expr_type(what_are_you).clone(),
                        Type::identifier("Int"),
                        what_are_you.clone(),
                    ));
                }
                _ => {}
            }
        }
        Expr::IfElse(condition, if_expr, else_expr) => {
            generate_type_equations(condition, type_map, type_equations);
            generate_type_equations(if_expr, type_map, type_equations);
            generate_type_equations(else_expr, type_map, type_equations);

            type_equations.push((
                type_map.must_get_expr_type(condition).clone(),
                Type::identifier("Bool"),
                *condition.clone(),
            ));
            type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                type_map.must_get_expr_type(if_expr).clone(),
                what_are_you.clone(),
            ));
            type_equations.push((
                type_map.must_get_expr_type(what_are_you).clone(),
                type_map.must_get_expr_type(else_expr).clone(),
                what_are_you.clone(),
            ));
        }
        Expr::Apply(func, arg) => {
            // generate_type_equations(func, type_map, type_equations);
            generate_type_equations(arg, type_map, type_equations);

            if let Some(function_type) = type_map.get_expr_type(func) {
                type_equations.push((
                    function_type.clone(),
                    Type::lambda(
                        type_map.must_get_expr_type(arg).clone(),
                        type_map.must_get_expr_type(what_are_you).clone(),
                    ),
                    what_are_you.clone(),
                ));
            }
        }
        Expr::Case(test_expr, branches) => {
            branches.iter().for_each(|alt| {
                let parse::Match::Simple(match_result_expr) = &alt.matches;
                generate_type_equations(match_result_expr, type_map, type_equations);

                type_equations.push((
                    type_map.must_get_expr_type(what_are_you).clone(),
                    type_map.must_get_expr_type(match_result_expr).clone(),
                    what_are_you.clone(),
                ));
                type_equations.push((
                    type_map.must_get_pattern_type(&alt.pattern).clone(),
                    type_map.must_get_expr_type(test_expr).clone(),
                    what_are_you.clone(),
                ));
            });
        }
        Expr::Paren(_) => todo!("Expr::Paren(_)"),
    }
}

fn pattern_to_expr(pattern: &Pattern) -> Option<Expr> {
    match pattern {
        Pattern::Literal(data) => Some(Expr::Literal(data.clone())),
        Pattern::Identifier(id) => Some(Expr::identifier(id)),
        Pattern::Tuple(args) => Some(Expr::tuple(
            args.iter().filter_map(pattern_to_expr).collect::<Vec<_>>(),
        )),
        Pattern::Constructor(_, _) => todo!("Pattern::Constructor(name, args)"),
        // Pattern::Constructor(name, args) => Some(Expr::application(
        //     vec![name],
        //     args.into_iter()
        //         .filter_map(pattern_to_expr)
        //         .collect::<Vec<_>>(),
        // )),
        Pattern::WildCard => todo!("Pattern::WildCard"),
    }
}

fn unify(left: Type, right: Type, substitutions: &mut LinkedHashMap<String, Type>) -> Option<()> {
    if left == right {
        return Some(());
    }

    match (&left, &right) {
        (Type::Variable(left_type_id), _) => {
            if let Some(inner_type) = substitutions.get(left_type_id) {
                return unify(inner_type.clone(), right, substitutions);
            }
            substitutions.insert(left_type_id.clone(), right);

            Some(())
        }
        (_, Type::Variable(right_type_id)) => {
            if let Some(inner_type) = substitutions.get(right_type_id) {
                return unify(inner_type.clone(), left, substitutions);
            }
            substitutions.insert(right_type_id.clone(), left);

            Some(())
        }
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return None;
            }
            ts1.iter().zip(ts2.iter()).for_each(|(t1, t2)| {
                unify(t1.clone(), t2.clone(), substitutions);
            });

            Some(())
        }
        (
            Type::Lambda {
                arg_type: arg1,
                return_type: return1,
            },
            Type::Lambda {
                arg_type: arg2,
                return_type: return2,
            },
        ) => {
            unify(*return1.clone(), *return2.clone(), substitutions);
            unify(*arg1.clone(), *arg2.clone(), substitutions);

            Some(())
        }
        (_, _) => None,
    }
}

fn apply_unifier<'a>(t: &'a Type, substitutions: &'a LinkedHashMap<String, Type>) -> Type {
    if substitutions.is_empty() {
        return t.clone();
    }

    match t {
        Type::Identifier(_) => t.clone(),
        Type::Atom(_) => todo!("Type::Atom(_)"),
        Type::Variable(type_id) => substitutions
            .get(type_id)
            .map(|inner_t| apply_unifier(inner_t, substitutions))
            .unwrap_or_else(|| t.clone()),
        Type::Lambda {
            arg_type,
            return_type,
        } => Type::lambda(
            apply_unifier(arg_type, substitutions),
            apply_unifier(return_type, substitutions),
        ),
        Type::Union { types } => {
            let types = types
                .iter()
                .map(|t| apply_unifier(t, substitutions))
                .unique()
                .collect::<Vec<_>>();

            match &types[..] {
                [] => panic!("empty union type"),
                [t] => t.clone(),
                _ => Type::union(types),
            }
        }
        Type::Tuple(types) => Type::tuple(
            types
                .iter()
                .map(|t| apply_unifier(t, substitutions))
                .collect(),
        ),
    }
}
