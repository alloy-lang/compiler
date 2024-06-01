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
    PatternRef {
        path: Path,
        scope: ScopeIdx,
    },
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
    ctx.add_pattern(pattern, &ast.syntax())
}

fn lower_pattern_inner(ctx: &mut LoweringCtx, ast: &ast::Pattern) -> Pattern {
    match ast {
        ast::Pattern::IntLiteral(lit) => {
            let Some(value) = lit.value() else {
                ctx.error(LoweringErrorKind::NumberLiteralTooLarge, ast.range());
                return Pattern::Missing;
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
                ctx.error(LoweringErrorKind::CharLiteralInvalid, ast.range());
                return Pattern::Missing;
            };
            Pattern::CharLiteral(value)
        }
        ast::Pattern::PatternRef(var) => Pattern::PatternRef {
            path: lower_pattern_ref(ctx, var),
            scope: ctx.scopes.current_scope(),
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
    let Some(ast_target) = destructure.target() else {
        unreachable!("parsing error")
    };

    let args = destructure
        .args()
        .iter()
        .map(|arg| lower_pattern(ctx, arg))
        .collect::<Vec<_>>();

    let target = lower_pattern_ref(ctx, &ast_target);

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

fn lower_pattern_ref(ctx: &mut LoweringCtx, var: &ast::PatternRef) -> Path {
    let Some(ast_path) = var.name() else {
        unreachable!("parsing error")
    };

    let Some(path) = ctx.resolve_reference_path(&ast_path, HirReferenceType::Pattern) else {
        unreachable!("parsing error")
    };

    path
}
