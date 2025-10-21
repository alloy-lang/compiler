use alloy_hir as hir;

mod hir_ty;
pub use hir_ty::*;

#[cfg(test)]
mod tests;

#[salsa::jar(db = HirTyDatabase)]
// pub struct Jar(infer_type);
pub struct Jar();

pub trait HirTyDatabase: salsa::DbWithJar<Jar> + hir::HirDatabase {
    fn upcast_hir_ty(&self) -> &dyn HirTyDatabase;
}

impl<DB> HirTyDatabase for DB
where
    DB: salsa::DbWithJar<Jar> + hir::HirDatabase,
{
    fn upcast_hir_ty(&self) -> &dyn HirTyDatabase {
        self
    }
}

// #[salsa::tracked]
// pub fn infer_type(db: &dyn HirTyDatabase, expression: hir::Expression) -> ResolvedType {
//     // let type_annotation = hir::type_annotation(db, &expression); // optional
//
//     let inferred_type = match expression {
//         hir::Expression::Missing => ResolvedType::Unknown,
//         hir::Expression::Literal(_) => todo!(),
//         hir::Expression::VariableRef { .. } => todo!(),
//         hir::Expression::Binary { .. } => todo!(),
//         hir::Expression::Unit => todo!(),
//         hir::Expression::IfThenElse { .. } => todo!(),
//         hir::Expression::Tuple(_) => todo!(),
//         hir::Expression::Unary { .. } => todo!(),
//         hir::Expression::Lambda { .. } => todo!(),
//         hir::Expression::FunctionCall { .. } => todo!(),
//         hir::Expression::Match { .. } => todo!(),
//     };
//
//     // if let Some(ta) = type_annotation {
//     //     let resolved_ta = to_resolved(ta);
//     //     if inferred_type != resolved_ta {
//     //         todo!("type mis-match");
//     return ResolvedType::Unknown;
//     // }
//     // }
//
//     return inferred_type;
// }
//
// #[cfg(test)]
// mod tests {
//     #[test]
//     fn fdanjklcdsa() {
//         let db = TestHirTyDatabase::default();
//     }
// }
