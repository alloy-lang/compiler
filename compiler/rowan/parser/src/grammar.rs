use alloy_rowan_lexer::TokenKind;
use alloy_rowan_syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseErrorContext;
use crate::parser::Parser;
use crate::parser::DEFAULT_RECOVERY_SET;
use crate::token_set::TokenSet;
use crate::ts;

mod argument;
mod expr;
mod ident;
mod import;
mod lambda;
mod module;
mod path;
mod stmt;
mod r#trait;
mod r#type;
mod type_variable;

pub(crate) fn source_file(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        if p.at(TokenKind::ModuleKw) {
            module::parse_module(p);
        } else {
            stmt::stmt(p);
        }
    }

    m.complete(p, SyntaxKind::SourceFile)
}
