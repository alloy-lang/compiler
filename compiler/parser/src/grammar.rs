use alloy_lexer::TokenKind;
use alloy_syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseErrorContext;
use crate::parser::Parser;
use crate::parser::DEFAULT_RECOVERY_SET;
use crate::token_set::TokenSet;
use crate::ts;

mod argument;
mod behavior;
mod expr;
mod ident;
mod import;
mod lambda;
mod module;
mod path;
mod stmt;
mod r#trait;
mod r#type;
mod type_annotation;
mod type_definition;
mod type_variable;
mod value;

pub(crate) fn source_file(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_eof() {
        if p.at(TokenKind::ModuleKw) {
            module::parse_module(p);
        } else {
            stmt::stmt(p);
        }
    }

    m.complete(p, SyntaxKind::SourceFile)
}
