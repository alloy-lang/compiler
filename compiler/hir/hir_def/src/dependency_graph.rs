//! Expression dependency analysis
//!
//! This module builds a dependency graph of expressions to enable proper
//! let-polymorphism with forward references. Expressions are processed in
//! topological order, with strongly-connected components handled together.
//!
//! Dependencies are collected incrementally during HIR lowering — each time
//! an expression is added, its direct dependencies are extracted from the
//! `Expression` value. Tarjan's algorithm is then run in `LoweringCtx::finish()`
//! to compute topological order with SCC grouping.

use crate::{BinaryOp, Expression, ExpressionIdx, Path, ResolutionIdx};
use rustc_hash::{FxHashMap, FxHashSet};

/// Extract the direct dependencies of an expression.
///
/// Returns the set of `ExpressionIdx` values that this expression directly depends on.
/// This includes both sub-expression indices (structural children) and path-based
/// references to other named expressions via `ResolutionIdx::Expression`.
///
/// No recursion is needed — each sub-expression has its own dependency entry
/// (recorded when it was lowered). Tarjan's algorithm handles transitivity.
pub(crate) fn extract_expression_deps(expr: &Expression) -> FxHashSet<ExpressionIdx> {
    let mut deps = FxHashSet::default();
    match expr {
        Expression::Missing | Expression::Literal(_) | Expression::Unit => {}
        Expression::VariableRef { path, .. } => {
            if let Path::ThisModule {
                resolution_idx: ResolutionIdx::Expression(id),
                ..
            } = path
            {
                deps.insert(*id);
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            deps.insert(*lhs);
            deps.insert(*rhs);
            if let BinaryOp::Custom(Path::ThisModule {
                resolution_idx: ResolutionIdx::Expression(id),
                ..
            }) = op
            {
                deps.insert(*id);
            }
        }
        Expression::IfThenElse {
            condition,
            then,
            else_,
        } => {
            deps.insert(*condition);
            deps.insert(*then);
            deps.insert(*else_);
        }
        Expression::Tuple(elems) => {
            for elem in elems {
                deps.insert(*elem);
            }
        }
        Expression::Unary { expression, .. } => {
            deps.insert(*expression);
        }
        Expression::Lambda { body, .. } => {
            deps.insert(*body);
        }
        Expression::FunctionCall { target, args, .. } => {
            if let Path::ThisModule {
                resolution_idx: ResolutionIdx::Expression(id),
                ..
            } = target
            {
                deps.insert(*id);
            }
            for arg in args {
                deps.insert(*arg);
            }
        }
        Expression::Match { condition, targets } => {
            deps.insert(*condition);
            for (_, body) in targets {
                deps.insert(*body);
            }
        }
    }
    deps
}

/// Dependency graph for expressions in a module
pub(crate) struct DependencyGraph {
    /// Map from expression to its direct dependencies
    dependencies: FxHashMap<ExpressionIdx, FxHashSet<ExpressionIdx>>,
    /// All expression IDs in insertion order
    all_expressions: Vec<ExpressionIdx>,
}

impl DependencyGraph {
    /// Build a dependency graph from pre-collected dependencies.
    pub(crate) fn new(dependencies: FxHashMap<ExpressionIdx, FxHashSet<ExpressionIdx>>) -> Self {
        let all_expressions = dependencies.keys().copied().collect();
        Self {
            dependencies,
            all_expressions,
        }
    }

    /// Compute a topological ordering of expressions with SCCs grouped together
    ///
    /// Returns groups of expressions where:
    /// - Each group is either a single expression or a strongly-connected component
    /// - Groups are in topological order (dependencies come before dependents)
    /// - Within an SCC, all expressions are mutually recursive
    pub fn topological_order(&self) -> Vec<Vec<ExpressionIdx>> {
        let mut state = TarjanState::new();

        for &expr_id in &self.all_expressions {
            if !state.visited.contains(&expr_id) {
                self.tarjan_visit(expr_id, &mut state);
            }
        }

        state.sccs
    }

    /// Tarjan's algorithm for finding strongly-connected components
    fn tarjan_visit(&self, expr_id: ExpressionIdx, state: &mut TarjanState) {
        let index = state.index;
        state.index += 1;
        state.indices.insert(expr_id, index);
        state.low_links.insert(expr_id, index);
        state.visited.insert(expr_id);
        state.stack.push(expr_id);
        state.on_stack.insert(expr_id);

        // Visit dependencies
        if let Some(deps) = self.dependencies.get(&expr_id) {
            for &dep_id in deps {
                if !state.visited.contains(&dep_id) {
                    self.tarjan_visit(dep_id, state);
                    let dep_lowlink = *state.low_links.get(&dep_id).unwrap();
                    let current_lowlink = state.low_links.get_mut(&expr_id).unwrap();
                    *current_lowlink = (*current_lowlink).min(dep_lowlink);
                } else if state.on_stack.contains(&dep_id) {
                    let dep_index = *state.indices.get(&dep_id).unwrap();
                    let current_lowlink = state.low_links.get_mut(&expr_id).unwrap();
                    *current_lowlink = (*current_lowlink).min(dep_index);
                }
            }
        }

        // If this is a root node, pop the SCC off the stack
        let is_root = state.indices.get(&expr_id) == state.low_links.get(&expr_id);
        if is_root {
            let mut scc = Vec::new();
            loop {
                let node = state.stack.pop().unwrap();
                state.on_stack.remove(&node);
                scc.push(node);
                if node == expr_id {
                    break;
                }
            }
            scc.reverse();
            state.sccs.push(scc);
        }
    }
}

/// State for Tarjan's algorithm
struct TarjanState {
    /// Counter for assigning DFS visit order numbers
    index: usize,
    /// Maps each expression to the order it was first visited during DFS
    indices: FxHashMap<ExpressionIdx, usize>,
    /// Maps each expression to the lowest index reachable from it
    low_links: FxHashMap<ExpressionIdx, usize>,
    /// Set of all expressions that have been visited
    visited: FxHashSet<ExpressionIdx>,
    /// Stack of expressions currently being explored in the DFS
    stack: Vec<ExpressionIdx>,
    /// Quick lookup to check if an expression is currently on the DFS stack
    on_stack: FxHashSet<ExpressionIdx>,
    /// The final result: list of SCCs in topological order
    sccs: Vec<Vec<ExpressionIdx>>,
}

impl TarjanState {
    fn new() -> Self {
        Self {
            index: 0,
            indices: FxHashMap::default(),
            low_links: FxHashMap::default(),
            visited: FxHashSet::default(),
            stack: Vec::new(),
            on_stack: FxHashSet::default(),
            sccs: Vec::new(),
        }
    }
}
