use alloy_rowan_lexer::TokenKind;
use alloy_rowan_syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

mod expr;
mod lambda;
mod stmt;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        stmt::stmt(p);
    }

    m.complete(p, SyntaxKind::Root)
}
