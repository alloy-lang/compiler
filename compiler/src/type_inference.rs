use itertools::Itertools;
use linked_hash_map::LinkedHashMap;

use crate::parse;
use crate::parse::{Expr, LiteralData, Pattern, Type};

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct TypeMap {
    map: LinkedHashMap<Expr, parse::Type>,
}

impl TypeMap {
    pub(crate) fn new() -> Self {
        TypeMap {
            map: LinkedHashMap::new(),
        }
    }

    fn must_get(&self, expr: &Expr) -> &parse::Type {
        self.get(expr)
            .unwrap_or_else(|| panic!("No type found for expressions '{:?}'", expr))
    }

    fn get(&self, expr: &Expr) -> Option<&parse::Type> {
        self.map.get(expr)
    }

    fn insert_type(&mut self, expr: &Expr, t: parse::Type) {
        self.map.entry(expr.clone()).or_insert(t);
    }

    pub(crate) fn insert_value_type(&mut self, name: &str, t: parse::Type) {
        self.map.entry(Expr::identifier(name)).or_insert(t);
    }

    fn insert_type_variable(&mut self, expr: &Expr) {
        let type_id = format!("T{}", self.map.len());

        self.map
            .entry(expr.clone())
            .or_insert(parse::Type::variable(type_id));
    }
}

pub(crate) fn infer_type(what_are_you: &Expr, context_type_map: &TypeMap) -> parse::Type {
    let type_map = {
        let mut type_map = context_type_map.clone();
        assign_type_names(what_are_you, &mut type_map);
        println!("type_map: {:?}\n\n", type_map);

        type_map
    };

    let type_equations = {
        let mut type_equations = Vec::new();
        generate_type_equations(what_are_you, &type_map, &mut type_equations);
        println!("type_equations: {:?}\n\n", type_equations);

        type_equations
    };

    let substitutions = {
        let mut substitutions = LinkedHashMap::new();

        type_equations
            .into_iter()
            .find(|(left, right, _original_expr)| {
                unify(left.clone(), right.clone(), &mut substitutions).is_none()
            });
        println!("substitutions: {:?}\n\n", substitutions);

        substitutions
    };

    apply_unifier(type_map.must_get(what_are_you), &substitutions)
}

fn assign_type_names<'a>(what_are_you: &'a Expr, type_map: &'a mut TypeMap) {
    match what_are_you {
        Expr::Literal(data) => match data {
            LiteralData::Integral(_) => {
                type_map.insert_type(what_are_you, parse::Type::identifier("Int"));
            }
            LiteralData::Fractional(_) => {
                type_map.insert_type(what_are_you, parse::Type::identifier("Float"));
            }
            LiteralData::String(_) => {
                type_map.insert_type(what_are_you, parse::Type::identifier("String"));
            }
            LiteralData::Char(_) => {
                type_map.insert_type(what_are_you, parse::Type::identifier("Char"));
            }
        },
        Expr::Identifier(_) => {
            type_map.insert_type_variable(what_are_you);
        }
        Expr::Lambda(arg, expr) => {
            type_map.insert_type_variable(what_are_you);

            match arg {
                Pattern::Literal(data) => assign_type_names(&Expr::Literal(data.clone()), type_map),
                Pattern::Identifier(id) => assign_type_names(&Expr::identifier(id), type_map),
                Pattern::Constructor(_, _) => todo!("Pattern::Constructor(_, _)"),
                Pattern::WildCard => todo!("Pattern::WildCard"),
            }

            assign_type_names(expr, type_map);
        }
        Expr::OpApply(left, _op, right) => {
            type_map.insert_type_variable(what_are_you);

            assign_type_names(left, type_map);
            assign_type_names(right, type_map);
        }
        Expr::IfElse(condition, if_expr, else_expr) => {
            type_map.insert_type_variable(what_are_you);

            assign_type_names(condition, type_map);
            assign_type_names(if_expr, type_map);
            assign_type_names(else_expr, type_map);
        }
        Expr::Apply(func, arg) => {
            type_map.insert_type_variable(what_are_you);

            assign_type_names(func, type_map);
            assign_type_names(arg, type_map);
        }
        Expr::Case(test_expr, branches) => {
            type_map.insert_type_variable(what_are_you);
            // type_map.insert_type(what_are_you, parse::Type::union(vec![]));

            assign_type_names(test_expr, type_map);

            branches
                .iter()
                .for_each(|alt| {
                    match &alt.pattern {
                        Pattern::Literal(data) => assign_type_names(&Expr::Literal(data.clone()), type_map),
                        Pattern::Identifier(_) => {}
                        Pattern::Constructor(_, _) => {}
                        Pattern::WildCard => {}
                    }

                    let parse::Match::Simple(match_result_expr) = &alt.matches;
                    assign_type_names(match_result_expr, type_map)
                });
        }
        Expr::Paren(expr) => {
            type_map.insert_type_variable(what_are_you);

            assign_type_names(expr, type_map);
        }
    };
}

fn generate_type_equations<'a>(
    what_are_you: &'a Expr,
    type_map: &TypeMap,
    type_equations: &mut Vec<(parse::Type, parse::Type, Expr)>,
) {
    match what_are_you {
        Expr::Literal(data) => match data {
            LiteralData::Integral(_) => type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::identifier("Int"),
                what_are_you.clone(),
            )),
            LiteralData::Fractional(_) => type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::identifier("Float"),
                what_are_you.clone(),
            )),
            LiteralData::String(_) => type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::identifier("String"),
                what_are_you.clone(),
            )),
            LiteralData::Char(_) => type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::identifier("Char"),
                what_are_you.clone(),
            )),
        },
        Expr::Identifier(_) => {
            // intentionally empty
        }
        Expr::Lambda(arg, expr) => {
            generate_type_equations(expr, type_map, type_equations);

            if let Some(arg_expr) = match arg {
                Pattern::Literal(data) => Some(Expr::Literal(data.clone())),
                Pattern::Identifier(id) => Some(Expr::identifier(id)),
                Pattern::Constructor(_, _) => todo!("Pattern::Constructor(_, _)"),
                Pattern::WildCard => todo!("Pattern::WildCard"),
            } {
                generate_type_equations(&arg_expr, type_map, type_equations);
                type_equations.push((
                    type_map.must_get(what_are_you).clone(),
                    parse::Type::lambda(
                        type_map.must_get(&arg_expr).clone(),
                        type_map.must_get(expr).clone(),
                    ),
                    what_are_you.clone(),
                ))
            }
        }
        Expr::OpApply(left, op, right) => {
            generate_type_equations(left, type_map, type_equations);
            generate_type_equations(right, type_map, type_equations);

            match op.as_str() {
                "==" | "!=" | "<" | "<=" | ">" | ">=" => {
                    type_equations.push((
                        type_map.must_get(what_are_you).clone(),
                        parse::Type::identifier("Bool"),
                        what_are_you.clone(),
                    ));
                }
                "+" | "-" | "*" | "/" => {
                    type_equations.push((
                        type_map.must_get(left).clone(),
                        parse::Type::identifier("Int"),
                        *left.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get(right).clone(),
                        parse::Type::identifier("Int"),
                        *right.clone(),
                    ));
                    type_equations.push((
                        type_map.must_get(what_are_you).clone(),
                        parse::Type::identifier("Int"),
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
                type_map.must_get(condition).clone(),
                parse::Type::identifier("Bool"),
                *condition.clone(),
            ));
            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                type_map.must_get(if_expr).clone(),
                what_are_you.clone(),
            ));
            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                type_map.must_get(else_expr).clone(),
                what_are_you.clone(),
            ));
        }
        Expr::Apply(func, arg) => {
            generate_type_equations(arg, type_map, type_equations);

            if let Some(function_type) = type_map.get(func) {
                type_equations.push((
                    function_type.clone(),
                    parse::Type::lambda(
                        type_map.must_get(arg).clone(),
                        type_map.must_get(what_are_you).clone(),
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
                    type_map.must_get(what_are_you).clone(),
                    type_map.must_get(match_result_expr).clone(),
                    what_are_you.clone(),
                ));

                if let Some(match_pattern_expr) = match &alt.pattern {
                    Pattern::Literal(data) => Some(Expr::Literal(data.clone())),
                    Pattern::Identifier(id) => Some(Expr::identifier(id)),
                    Pattern::Constructor(name, args) => Some(Expr::application(
                        vec![name],
                        args.into_iter().map(|p| match p {
                            Pattern::Literal(data) => Expr::Literal(data.clone()),
                            Pattern::Identifier(_) => todo!("Pattern::Identifier(_)"),
                            Pattern::Constructor(_, _) => todo!("Pattern::Constructor(_, _)"),
                            Pattern::WildCard => todo!("Pattern::WildCard"),
                        })
                            .collect::<Vec<_>>(),
                    )),
                    Pattern::WildCard => todo!("Pattern::WildCard"),
                } {
                    generate_type_equations(&match_pattern_expr, type_map, type_equations);
                    type_equations.push((
                        type_map.must_get(&match_pattern_expr).clone(),
                        type_map.must_get(test_expr).clone(),
                        what_are_you.clone(),
                    ))
                }
            });
        }
        Expr::Paren(_) => todo!("Expr::Paren(_)"),
    }
}

fn unify(
    left: parse::Type,
    right: parse::Type,
    substitutions: &mut LinkedHashMap<String, parse::Type>,
) -> Option<()> {
    if left == right {
        return Some(());
    }

    match (&left, &right) {
        (parse::Type::Variable(left_type_id), _) => {
            if let Some(inner_type) = substitutions.get(left_type_id) {
                return unify(inner_type.clone(), right, substitutions);
            }
            substitutions.insert(left_type_id.clone(), right);

            Some(())
        }
        (_, parse::Type::Variable(right_type_id)) => {
            if let Some(inner_type) = substitutions.get(right_type_id) {
                return unify(inner_type.clone(), left, substitutions);
            }
            substitutions.insert(right_type_id.clone(), left);

            Some(())
        }
        (parse::Type::Tuple(ts1), parse::Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return None;
            }
            ts1.iter().zip(ts2.iter()).for_each(|(t1, t2)| {
                unify(t1.clone(), t2.clone(), substitutions);
            });

            Some(())
        }
        (
            parse::Type::Lambda {
                arg_type: arg1,
                return_type: return1,
            },
            parse::Type::Lambda {
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

fn apply_unifier<'a>(
    t: &'a parse::Type,
    substitutions: &'a LinkedHashMap<String, parse::Type>,
) -> parse::Type {
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
        } => parse::Type::lambda(
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
                _ => parse::Type::union(types),
            }
        }
        Type::Tuple(types) => parse::Type::tuple(
            types
                .iter()
                .map(|t| apply_unifier(t, substitutions))
                .collect(),
        ),
    }
}
