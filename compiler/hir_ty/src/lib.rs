use alloy_hir as hir;

mod hir_ty;
pub use hir_ty::*;

mod resolution;
pub use resolution::*;

#[cfg(test)]
mod tests;

#[salsa::db]
pub trait HirTyDatabase: hir::HirDatabase {}
