use itertools::Itertools;
use linked_hash_map::LinkedHashMap;

use crate::parse;
use crate::parse::{BinOp, Expr, Type};

#[derive(Debug, Eq, PartialEq, Clone)]
struct TypeMap {
    map: LinkedHashMap<Expr, parse::Type>,
}

impl TypeMap {
    fn new() -> Self {
        TypeMap {
            map: LinkedHashMap::new(),
        }
    }

    fn must_get(&self, expr: &Expr) -> &parse::Type {
        &self.map[expr]
    }

    fn get(&self, expr: &Expr) -> Option<&parse::Type> {
        self.map.get(expr)
    }

    fn insert_type(&mut self, expr: &Expr, t: parse::Type) {
        self.map.entry(expr.clone()).or_insert(t);
    }

    fn insert_type_variable(&mut self, expr: &Expr) {
        let type_id = format!("T{}", self.map.len());

        self.map
            .entry(expr.clone())
            .or_insert(parse::Type::variable(type_id));
    }
}

pub(crate) fn infer_type(what_are_you: &Expr) -> parse::Type {
    let type_map = {
        let mut type_map = TypeMap::new();
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

    let inferred_type = {
        let mut substitutions = LinkedHashMap::new();

        for (left, right, _original_expr) in type_equations {
            match unify(left, right, &mut substitutions) {
                None => {
                    break;
                }
                Some(new_subs) => {
                    substitutions = new_subs.clone();
                }
            }
        }
        println!("substitutions: {:?}\n\n", substitutions);

        apply_unifier(&type_map.must_get(what_are_you), &substitutions)
    };

    inferred_type
}

fn assign_type_names<'a>(what_are_you: &'a Expr, type_map: &'a mut TypeMap) {
    match what_are_you {
        Expr::StringLiteral(_) => {
            type_map.insert_type(what_are_you, parse::Type::identifier("String"));
        }
        Expr::FloatLiteral(_) => {
            type_map.insert_type(what_are_you, parse::Type::identifier("Float"));
        }
        Expr::IntLiteral(_) => {
            type_map.insert_type(what_are_you, parse::Type::identifier("Int"));
        }
        Expr::Identifier(_) => {
            type_map.insert_type_variable(what_are_you);
        }
        Expr::Function(args, expr) => {
            type_map.insert_type_variable(what_are_you);

            args.iter().for_each(|arg| {
                assign_type_names(arg, type_map);
            });

            assign_type_names(expr, type_map);
        }
        Expr::BinOp(_op, left, right) => {
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
        Expr::Call(_address, args) => {
            type_map.insert_type_variable(what_are_you);

            args.iter().for_each(|arg| {
                assign_type_names(arg, type_map);
            });
        }
        Expr::Tuple(exprs) => {
            type_map.insert_type_variable(what_are_you);

            exprs.iter().for_each(|expr| {
                assign_type_names(expr, type_map);
            });
        }
        Expr::Match(exprs) => {
            type_map.insert_type_variable(what_are_you);

            exprs.iter().for_each(|expr| {
                assign_type_names(expr, type_map);
            });
        }
    };
}

fn generate_type_equations<'a>(
    what_are_you: &'a Expr,
    type_map: &TypeMap,
    type_equations: &mut Vec<(parse::Type, parse::Type, &'a Expr)>,
) {
    match what_are_you {
        Expr::StringLiteral(_) => type_equations.push((
            type_map.must_get(what_are_you).clone(),
            parse::Type::identifier("String"),
            what_are_you,
        )),
        Expr::FloatLiteral(_) => type_equations.push((
            type_map.must_get(what_are_you).clone(),
            parse::Type::identifier("Float"),
            what_are_you,
        )),
        Expr::IntLiteral(_) => type_equations.push((
            type_map.must_get(what_are_you).clone(),
            parse::Type::identifier("Int"),
            what_are_you,
        )),
        Expr::Identifier(_) => {
            // intentionally empty
        }
        Expr::Function(args, expr) => {
            generate_type_equations(expr, type_map, type_equations);
            // TODO 002: remove assumption about argument length
            generate_type_equations(&args[0], type_map, type_equations);

            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::lambda(
                    // TODO 002: remove assumption about argument length
                    type_map.must_get(&args[0]).clone(),
                    type_map.must_get(expr).clone(),
                ),
                what_are_you,
            ))
        }
        Expr::BinOp(op, left, right) => {
            generate_type_equations(left, type_map, type_equations);
            generate_type_equations(right, type_map, type_equations);

            type_equations.push((
                type_map.must_get(left).clone(),
                parse::Type::identifier("Int"),
                left,
            ));
            type_equations.push((
                type_map.must_get(right).clone(),
                parse::Type::identifier("Int"),
                right,
            ));

            match op {
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                    type_equations.push((
                        type_map.must_get(what_are_you).clone(),
                        parse::Type::identifier("Bool"),
                        what_are_you,
                    ));
                }
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    type_equations.push((
                        type_map.must_get(what_are_you).clone(),
                        parse::Type::identifier("Int"),
                        what_are_you,
                    ));
                }
            }
        }
        Expr::IfElse(condition, if_expr, else_expr) => {
            generate_type_equations(condition, type_map, type_equations);
            generate_type_equations(if_expr, type_map, type_equations);
            generate_type_equations(else_expr, type_map, type_equations);

            type_equations.push((
                type_map.must_get(condition).clone(),
                parse::Type::identifier("Bool"),
                condition,
            ));
            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                type_map.must_get(if_expr).clone(),
                what_are_you,
            ));
            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                type_map.must_get(else_expr).clone(),
                what_are_you,
            ));
        }
        Expr::Call(address, args) => {
            args.iter()
                .for_each(|arg| generate_type_equations(arg, type_map, type_equations));

            if let Some(function_type) = type_map.get(&Expr::identifier(address.join("::"))) {
                type_equations.push((
                    function_type.clone(),
                    parse::Type::lambda(
                        // TODO 002: remove assumption about argument length
                        type_map.must_get(&args[0]).clone(),
                        type_map.must_get(what_are_you).clone(),
                    ),
                    what_are_you,
                ));
            }
        }
        Expr::Tuple(exprs) => {
            exprs
                .iter()
                .for_each(|expr| generate_type_equations(expr, type_map, type_equations));

            type_equations.push((
                type_map.must_get(what_are_you).clone(),
                parse::Type::tuple(
                    exprs
                        .iter()
                        .map(|expr| type_map.must_get(expr).clone())
                        .collect(),
                ),
                what_are_you,
            ))
        }
        Expr::Match(exprs) => {
            exprs.iter().for_each(|expr| {
                generate_type_equations(expr, type_map, type_equations);

                type_equations.push((
                    type_map.must_get(what_are_you).clone(),
                    type_map.must_get(expr).clone(),
                    what_are_you,
                ))
            });
        }
    }
}

fn unify(
    left: parse::Type,
    right: parse::Type,
    substitutions: &mut LinkedHashMap<String, parse::Type>,
) -> Option<&LinkedHashMap<String, parse::Type>> {
    if left == right {
        return Some(substitutions);
    }

    match (&left, &right) {
        (parse::Type::Variable(left_type_id), _) => {
            if let Some(inner_type) = substitutions.get(left_type_id) {
                return unify(inner_type.clone(), right, substitutions);
            }
            substitutions.insert(left_type_id.clone(), right);

            Some(substitutions)
        }
        (_, parse::Type::Variable(right_type_id)) => {
            if let Some(inner_type) = substitutions.get(right_type_id) {
                return unify(inner_type.clone(), left, substitutions);
            }
            substitutions.insert(right_type_id.clone(), left);

            Some(substitutions)
        }
        (parse::Type::Tuple(ts1), parse::Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                return None;
            }
            ts1.iter().zip(ts2.iter()).for_each(|(t1, t2)| {
                unify(t1.clone(), t2.clone(), substitutions);
            });

            Some(substitutions)
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

            Some(substitutions)
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
                .into_iter()
                .map(|t| apply_unifier(t, substitutions))
                .unique()
                .collect::<Vec<_>>();

            match types.len() {
                0 => todo!("empty union type"),
                1 => types[0].clone(),
                _ => parse::Type::union(types),
            }
        }
        Type::Tuple(types) => parse::Type::tuple(
            types
                .into_iter()
                .map(|t| apply_unifier(t, substitutions))
                .collect(),
        ),
    }
}
