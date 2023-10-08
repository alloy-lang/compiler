use alloy_syntax::SyntaxNode;
pub use ast::*;
mod ast;
#[cfg(test)]
mod tests;
pub mod validation;

#[must_use]
pub fn source_file(syntax: SyntaxNode) -> Option<SourceFile> {
    SourceFile::cast(syntax)
}
