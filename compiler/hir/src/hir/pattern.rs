#[allow(clippy::wildcard_imports)]
use super::*;

#[allow(clippy::module_name_repetitions)]
pub type PatternIdx = Idx<Pattern>;

#[derive(Debug)]
pub enum Pattern {
    Missing,
    IntLiteral(u64),
    FractionLiteral(NotNan<f64>),
    StringLiteral(String),
    CharLiteral(char),
    VariableRef { name: Path },
    VariableDeclaration { name: Path },
    Nil,
    Destructure { target: Path, args: Vec<PatternIdx> },
    Unit,
    Tuple(NonEmpty<PatternIdx>),
}

pub(super) fn lower_pattern(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> PatternIdx {
    let pattern = lower_pattern_inner(ctx, ast);
    ctx.add_pattern(pattern, &ast.syntax())
}

fn lower_pattern_inner(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> Pattern {
    match ast {
        ast::Pattern::IntLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::IntLiteral(value)
        }
        ast::Pattern::FractionLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::FractionLiteral(value)
        }
        ast::Pattern::StringLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::StringLiteral(value)
        }
        ast::Pattern::CharLiteral(lit) => {
            let Some(value) = lit.value() else {
                unreachable!("parsing error")
            };
            Pattern::CharLiteral(value)
        }
        ast::Pattern::VariableRef(var) => Pattern::VariableRef {
            name: lower_variable_ref(ctx, var),
        },
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
                    None => ctx.add_missing_pattern(&arg.syntax()),
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
    let Some(target) = destructure
        .target()
        .map(|target| lower_variable_ref(ctx, &target))
    else {
        unreachable!("parsing error")
    };

    let args = destructure
        .args()
        .iter()
        .map(|arg| lower_pattern(ctx, arg))
        .collect::<Vec<_>>();

    Pattern::Destructure { target, args }
}

pub(super) fn lower_variable_declaration(var: &ast::VariableDeclaration) -> Path {
    let Some(path) = var.name() else {
        unreachable!("parsing error")
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        unreachable!("parsing error")
    };

    name
}

pub(super) fn lower_variable_ref(ctx: &mut LoweringCtx, var: &ast::VariableRef) -> Path {
    let Some(path) = var.name() else {
        unreachable!("parsing error")
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        unreachable!("parsing error")
    };

    if !ctx.contains_variable_ref(&name) {
        ctx.error(
            LoweringErrorKind::UnknownReference { path: name.clone() },
            var.range(),
        );
    }

    name
}
