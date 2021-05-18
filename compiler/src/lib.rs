use std::collections::HashMap;
use crate::CompileError::BadDocs;

mod parse;
mod canonical;

pub struct Artifacts {
    module: HashMap<String, String>,
}

pub enum CompileError {
    BadSyntax(),
    BadImports(),
    BadNames(),
    BadTypes(),
    BadPatterns(),
    BadDocs(),
}

pub fn compile() -> Result<Artifacts, CompileError> {
    let canonical = canonicalize()?;

    Ok(Artifacts {
        module: HashMap::default(),
    })
}

fn canonicalize() -> Result<canonical::Module, CompileError> {
    Err(BadDocs())
}
