#[allow(clippy::wildcard_imports)]
use super::*;

pub type PatternIdx = Idx<Pattern>;

#[derive(Debug)]
pub enum Pattern {
    Missing,
    IntLiteral(u64),
    FractionLiteral(NotNan<f64>),
    StringLiteral(String),
    CharLiteral(char),
    VariableRef { name: Path },
    NilIdentifier,
    Destructor { target: Path, args: Vec<PatternIdx> },
    Unit,
    Paren(PatternIdx),
    Tuple(NonEmpty<PatternIdx>),
}

pub(super) fn lower_pattern(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> PatternIdx {
    let pattern = lower_pattern_inner(ctx, ast);
    ctx.pattern(pattern, &ast.syntax())
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
            let name = match lower_variable_ref(var) {
                Ok(value) => value,
                Err(value) => return value,
            };

            Pattern::VariableRef { name }
        }
        ast::Pattern::NilIdentifier(_) => Pattern::NilIdentifier,
        ast::Pattern::Destructor(destructor) => {
            let Some(target) = destructor.target() else {
                todo!("validation");
                return Pattern::Missing;
            };
            let Ok(target) = lower_variable_ref(&target) else {
                todo!("validation");
                return Pattern::Missing;
            };
            let args = destructor
                .args()
                .iter()
                .map(|arg| lower_pattern(ctx, &arg))
                .collect::<Vec<_>>();
            Pattern::Destructor { target, args }
        }
        ast::Pattern::Unit(_) => Pattern::Unit,
        ast::Pattern::ParenPattern(paren) => {
            let Some(inner) = paren.pattern() else {
                todo!("validation");
                return Pattern::Missing;
            };
            let Some(inner) = inner.arg() else {
                todo!("validation");
                return Pattern::Missing;
            };

            lower_pattern_inner(ctx, &inner)
        }
        ast::Pattern::TuplePattern(tuple) => {
            let args = tuple
                .patterns()
                .iter()
                .map(|arg| {
                    let Some(arg) = arg.arg() else {
                        todo!("validation");
                        return ctx.pattern(Pattern::Missing, &arg.syntax());
                    };
                    lower_pattern(ctx, &arg)
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

pub(super) fn lower_variable_ref(var: &ast::VariableRef) -> Result<Path, Pattern> {
    let Some(path) = var.name() else {
        todo!("validation");
        return Err(Pattern::Missing);
    };

    let Ok(name) = Path::try_from(path.segments()) else {
        todo!("validation");
        return Err(Pattern::Missing);
    };
    Ok(name)
}
