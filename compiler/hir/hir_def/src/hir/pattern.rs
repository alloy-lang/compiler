#[allow(clippy::wildcard_imports)]
use super::*;

#[allow(clippy::module_name_repetitions)]
pub type PatternIdx = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Missing,
    Literal(Literal),
    VariableDeclaration {
        name: Name,
    },
    Nil,
    Destructure {
        target: Path,
        scope: ScopeIdx,
        args: Vec<PatternIdx>,
    },
    Unit,
    Tuple(NonEmpty<PatternIdx>),
}

pub(super) fn lower_pattern(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> PatternIdx {
    let pattern = lower_pattern_inner(ctx, ast);
    ctx.add_pattern(pattern, ast)
}

fn lower_pattern_inner(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> Pattern {
    match ast {
        ast::Pattern::IntLiteral(lit) => {
            let Some(value) = lit.value() else {
                ctx.error(LoweringErrorKind::NumberLiteralTooLarge, ast.range());
                return Pattern::Missing;
            };
            Pattern::Literal(Literal::Int(value))
        }
        ast::Pattern::FractionLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::Literal(Literal::Fraction(value))
        }
        ast::Pattern::StringLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::Literal(Literal::String(value))
        }
        ast::Pattern::CharLiteral(lit) => {
            let Some(value) = lit.value() else {
                ctx.error(LoweringErrorKind::CharLiteralInvalid, ast.range());
                return Pattern::Missing;
            };
            Pattern::Literal(Literal::Char(value))
        }
        ast::Pattern::VariableDeclaration(var) => Pattern::VariableDeclaration {
            name: lower_variable_declaration(var),
        },
        ast::Pattern::NilIdentifier(_) => Pattern::Nil,
        ast::Pattern::Destructure(destructure) => lower_destructure(ctx, destructure),
        ast::Pattern::Unit(_) => Pattern::Unit,
        ast::Pattern::ParenPattern(paren) => paren
            .pattern()
            .and_then(|inner| inner.arg())
            .map_or(Pattern::Missing, |inner| lower_pattern_inner(ctx, &inner)),
        ast::Pattern::TuplePattern(tuple) => {
            let args = tuple
                .patterns()
                .iter()
                .map(|arg| match arg.arg() {
                    Some(arg) => lower_pattern(ctx, &arg),
                    None => ctx.add_missing_pattern(arg),
                })
                .collect::<Vec<_>>();

            let Ok(args) = NonEmpty::try_from(args) else {
                unreachable!("parsing error")
            };

            Pattern::Tuple(args)
        }
    }
}

fn lower_destructure(ctx: &mut LoweringCtx, destructure: &ast::Destructure) -> Pattern {
    let Some(ast_path) = destructure.target() else {
        unreachable!("parsing error")
    };

    match ast_path.segments().join("::").as_str() {
        "True" => {
            return Pattern::Literal(Literal::Bool(true));
        }
        "False" => {
            return Pattern::Literal(Literal::Bool(false));
        }
        _ => {}
    }

    let Some(target) = ctx.resolve_reference_path(&ast_path, HirReferenceType::Pattern) else {
        unreachable!("parsing error")
    };

    let args = destructure
        .args()
        .iter()
        .map(|arg| lower_pattern(ctx, arg))
        .collect::<Vec<_>>();

    Pattern::Destructure {
        target,
        scope: ctx.scopes.current_scope(),
        args,
    }
}

pub(super) fn lower_variable_declaration(var: &ast::VariableDeclaration) -> Name {
    let Some(name) = var.name() else {
        unreachable!("parsing error")
    };

    Name::new(name.text())
}
