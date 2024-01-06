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
                todo!("validation");
                return Pattern::Missing;
            };
            Pattern::IntLiteral(value)
        }
        ast::Pattern::FractionLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Pattern::Missing;
            };
            Pattern::FractionLiteral(value)
        }
        ast::Pattern::StringLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Pattern::Missing;
            };
            Pattern::StringLiteral(value)
        }
        ast::Pattern::CharLiteral(lit) => {
            let Some(value) = lit.value() else {
                todo!("validation");
                return Pattern::Missing;
            };
            Pattern::CharLiteral(value)
        }
        ast::Pattern::VariableRef(var) => {
            let Some(name) = lower_variable_ref(var) else {
                todo!("validation");
                return Pattern::Missing;
            };

            Pattern::VariableRef { name }
        }
        ast::Pattern::NilIdentifier(_) => Pattern::Nil,
        ast::Pattern::Destructure(destructure) => {
            let Some(target) = destructure
                .target()
                .and_then(|target| lower_variable_ref(&target))
            else {
                todo!("validation");
                return Pattern::Missing;
            };

            let args = destructure
                .args()
                .iter()
                .map(|arg| lower_pattern(ctx, arg))
                .collect::<Vec<_>>();
            Pattern::Destructure { target, args }
        }
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
                todo!("validation");
                return Pattern::Missing;
            };

            Pattern::Tuple(args)
        }
    }
}

pub(super) fn lower_variable_ref(var: &ast::VariableRef) -> Option<Path> {
    let Some(path) = var.name() else {
        todo!("validation");
        return None;
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        todo!("validation");
        return None;
    };
    Some(name)
}
