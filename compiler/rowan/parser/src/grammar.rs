use alloy_rowan_lexer::TokenKind;
use alloy_rowan_syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseErrorContext;
use crate::parser::Parser;

mod expr;
mod import;
mod lambda;
mod stmt;

pub(crate) fn source_file(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        stmt::stmt(p);
    }

    m.complete(p, SyntaxKind::SourceFile)
}
