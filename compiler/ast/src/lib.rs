use alloy_parser::{ParseError, ParseTree};
use alloy_workspace::ModuleId;
pub use ast::*;
mod ast;
#[cfg(test)]
mod tests;

#[salsa::db]
pub trait AstDatabase: alloy_workspace::WorkspaceDatabase {}

pub fn parse_source_file(
    db: &dyn AstDatabase,
    module_id: ModuleId,
) -> (Option<SourceFile>, Vec<ParseError>) {
    let source = db.get_source(module_id);

    let parse_tree = parse_source_file_inner(db, *source);
    let syntax = parse_tree.syntax();

    (SourceFile::cast(syntax), parse_tree.errors().to_vec())
}

#[salsa::tracked]
fn parse_source_file_inner<'db>(
    db: &'db dyn AstDatabase,
    source: alloy_workspace::RawSourceFile,
) -> ParseTree {
    alloy_parser::parse_source_file(source.contents(db))
}

#[must_use]
#[cfg(test)]
pub(crate) fn source_file(raw: &str) -> (Option<SourceFile>, Vec<ParseError>) {
    let parse_tree = alloy_parser::parse_source_file(raw);
    let syntax = parse_tree.syntax();

    (SourceFile::cast(syntax), parse_tree.errors().to_vec())
}
