use crate::canonical::canonicalize;
use crate::parse::parser;

mod canonical;
#[allow(clippy::redundant_closure_call)]
mod parse;
mod type_inference;

#[cfg(test)]
mod test_source;
mod types;

// pub(crate) enum CompileError {
//     BadSyntax(),
//     BadImports(),
//     BadNames(),
//     BadTypes(),
//     BadPatterns(),
//     BadDocs(),
// }
//
// pub fn compile() -> Result<Artifacts, CompileError> {
//     let canonical = canonicalize()?;
//
//     Ok(Artifacts {
//         module: HashMap::default(),
//     })
// }
//
pub fn compile(source: &str) -> canonical::Module {
    let parsed_module = parser::module(source).unwrap();

    canonicalize(parsed_module).unwrap()
}
