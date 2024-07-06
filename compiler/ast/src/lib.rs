use alloy_parser::ParseError;
pub use ast::*;
mod ast;
#[cfg(test)]
mod tests;

#[must_use]
pub fn source_file(raw: &str) -> (Option<SourceFile>, Vec<ParseError>) {
    let parse = alloy_parser::parse_source_file(raw);
    let syntax = parse.syntax();

    (SourceFile::cast(syntax), parse.errors().to_vec())
}
