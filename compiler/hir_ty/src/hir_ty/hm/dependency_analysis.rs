//! Dependency analysis for expressions
//!
//! This module builds a dependency graph of expressions to enable proper
//! let-polymorphism with forward references. Expressions are processed in
//! topological order, with strongly-connected components handled together.

use alloy_hir as hir;
use alloy_hir_resolved::{self as res, EPFql, EPTdFql, Fql};
use alloy_workspace::ModuleId;
use rustc_hash::{FxHashMap, FxHashSet};

/// Dependency graph for expressions in a module
pub struct DependencyGraph {
    /// Map from expression to its direct dependencies
    dependencies: FxHashMap<hir::ExpressionIdx, FxHashSet<hir::ExpressionIdx>>,
    /// All expression IDs in the module
    all_expressions: Vec<hir::ExpressionIdx>,
}

impl DependencyGraph {
    /// Build a dependency graph for all expressions in a module
    pub fn build(db: &dyn crate::HirTyDatabase, module_id: ModuleId) -> Self {
        let (hir_module, _) = hir::lower_file(db, module_id);

        let mut dependencies = FxHashMap::default();
        let mut all_expressions = Vec::new();

        for (expr_id, _expr, _range, _name) in hir_module.expressions() {
            all_expressions.push(expr_id);

            // Collect dependencies for this expression
            let deps = collect_expression_dependencies(db, module_id, expr_id);
            dependencies.insert(expr_id, deps);
        }

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
    pub fn topological_order(&self) -> Vec<Vec<hir::ExpressionIdx>> {
        // Use Tarjan's algorithm for finding SCCs in topological order
        let mut state = TarjanState::new();

        for &expr_id in &self.all_expressions {
            if !state.visited.contains(&expr_id) {
                self.tarjan_visit(expr_id, &mut state);
            }
        }

        // Tarjan produces SCCs in correct topological order (dependencies first)
        // Do NOT reverse - the completion order already gives us the right order
        state.sccs
    }

    /// Tarjan's algorithm for finding strongly-connected components
    fn tarjan_visit(&self, expr_id: hir::ExpressionIdx, state: &mut TarjanState) {
        let index = state.index;
        state.index += 1;
        state.indices.insert(expr_id, index);
        state.lowlinks.insert(expr_id, index);
        state.visited.insert(expr_id);
        state.stack.push(expr_id);
        state.on_stack.insert(expr_id);

        // Visit dependencies
        if let Some(deps) = self.dependencies.get(&expr_id) {
            for &dep_id in deps {
                if !state.visited.contains(&dep_id) {
                    self.tarjan_visit(dep_id, state);
                    let dep_lowlink = *state.lowlinks.get(&dep_id).unwrap();
                    let current_lowlink = state.lowlinks.get_mut(&expr_id).unwrap();
                    *current_lowlink = (*current_lowlink).min(dep_lowlink);
                } else if state.on_stack.contains(&dep_id) {
                    let dep_index = *state.indices.get(&dep_id).unwrap();
                    let current_lowlink = state.lowlinks.get_mut(&expr_id).unwrap();
                    *current_lowlink = (*current_lowlink).min(dep_index);
                }
            }
        }

        // If this is a root node, pop the SCC off the stack
        let is_root = state.indices.get(&expr_id) == state.lowlinks.get(&expr_id);
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
            // SCCs are built in reverse order, so reverse to get proper order
            scc.reverse();
            state.sccs.push(scc);
        }
    }
}

/// State for Tarjan's algorithm
struct TarjanState {
    /// Counter for assigning DFS visit order numbers (incremented for each new node visited)
    index: usize,

    /// Maps each expression to the order it was first visited during DFS
    /// Used to track when we first discovered each node
    indices: FxHashMap<hir::ExpressionIdx, usize>,

    /// Maps each expression to the lowest index reachable from it
    /// This is the key to identifying SCC roots: a node is an SCC root if lowlink[node] == index[node]
    lowlinks: FxHashMap<hir::ExpressionIdx, usize>,

    /// Set of all expressions that have been visited
    /// Used to avoid revisiting nodes we've already processed
    visited: FxHashSet<hir::ExpressionIdx>,

    /// Stack of expressions currently being explored in the DFS
    /// When we find an SCC root, we pop all its members off this stack
    stack: Vec<hir::ExpressionIdx>,

    /// Quick lookup to check if an expression is currently on the DFS stack
    /// Used to distinguish between back edges (to nodes on stack) and cross edges (to completed nodes)
    on_stack: FxHashSet<hir::ExpressionIdx>,

    /// The final result: list of strongly connected components (SCCs) in topological order
    /// Each inner Vec contains the expressions that form one SCC (mutually recursive group)
    sccs: Vec<Vec<hir::ExpressionIdx>>,
}

impl TarjanState {
    fn new() -> Self {
        Self {
            index: 0,
            indices: FxHashMap::default(),
            lowlinks: FxHashMap::default(),
            visited: FxHashSet::default(),
            stack: Vec::new(),
            on_stack: FxHashSet::default(),
            sccs: Vec::new(),
        }
    }
}

/// Collect all expression dependencies (expressions this one references)
fn collect_expression_dependencies(
    db: &dyn crate::HirTyDatabase,
    module_id: ModuleId,
    expr_id: hir::ExpressionIdx,
) -> FxHashSet<hir::ExpressionIdx> {
    let mut deps = FxHashSet::default();

    // Resolve the expression
    let expr = match res::resolve_expression_by_id(db, module_id, expr_id) {
        Ok(e) => e,
        Err(_) => return deps, // Resolution error, no dependencies
    };

    // Walk the expression tree and collect dependencies
    collect_from_expression(db, module_id, &expr, &mut deps);

    deps
}

/// Recursively collect dependencies from an expression
fn collect_from_expression(
    db: &dyn crate::HirTyDatabase,
    module_id: ModuleId,
    expr: &res::Expression,
    deps: &mut FxHashSet<hir::ExpressionIdx>,
) {
    match expr {
        res::Expression::VariableRef(ref_fql) => {
            // Only track dependencies within the same module
            if let EPFql::Expression(expr_fql) = ref_fql {
                if expr_fql.module_id == module_id {
                    deps.insert(expr_fql.local_id);
                }
            }
            // Pattern references don't create expression dependencies
        }
        res::Expression::Lambda { args, body } => {
            // Lambda body may reference other expressions
            collect_from_fql(db, module_id, body, deps);
            // Args are patterns - they don't create expression dependencies
            let _ = args;
        }
        res::Expression::FunctionCall {
            target,
            variant_name: _,
            args,
        } => {
            // Target might be an expression reference
            match target {
                EPTdFql::Expression(expr_fql) => {
                    if expr_fql.module_id == module_id {
                        deps.insert(expr_fql.local_id);
                    }
                }
                EPTdFql::Pattern(_) => {} // Pattern refs don't create dependencies
                EPTdFql::TypeDefinition(_) => {} // Type Defs don't create dependencies
            }
            // Recursively check arguments
            for arg in args {
                collect_from_fql(db, module_id, arg, deps);
            }
        }
        res::Expression::Binary { lhs, rhs, .. } => {
            collect_from_fql(db, module_id, lhs, deps);
            collect_from_fql(db, module_id, rhs, deps);
        }
        res::Expression::Unary { expression, .. } => {
            collect_from_fql(db, module_id, expression, deps);
        }
        res::Expression::Tuple(elements) => {
            for elem in elements {
                collect_from_fql(db, module_id, elem, deps);
            }
        }
        res::Expression::IfThenElse {
            condition,
            then,
            else_,
        } => {
            collect_from_fql(db, module_id, condition, deps);
            collect_from_fql(db, module_id, then, deps);
            collect_from_fql(db, module_id, else_, deps);
        }
        res::Expression::Match { condition, targets } => {
            collect_from_fql(db, module_id, condition, deps);
            for (_pattern, body) in targets {
                // Patterns don't create expression dependencies
                collect_from_fql(db, module_id, body, deps);
            }
        }
        // These don't have dependencies on other expressions
        res::Expression::Literal(_)
        | res::Expression::Unit
        | res::Expression::VariantConstructor { .. }
        | res::Expression::AbstractTraitMemberRef { .. }
        | res::Expression::Missing => {}
    }
}

/// Helper to collect dependencies from an expression FQL
fn collect_from_fql(
    db: &dyn crate::HirTyDatabase,
    module_id: ModuleId,
    fql: &Fql<hir::Expression>,
    deps: &mut FxHashSet<hir::ExpressionIdx>,
) {
    // If it's in the same module, add as dependency
    if fql.module_id == module_id {
        deps.insert(fql.local_id);
    }

    // Also recursively analyze this expression
    if let Ok(expr) = res::resolve_expression_by_id(db, fql.module_id, fql.local_id) {
        collect_from_expression(db, module_id, &expr, deps);
    }
}
