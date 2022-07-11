use rowan::GreenNode;

use alloy_rowan_lexer::Lexer;
use alloy_rowan_syntax::SyntaxNode;

use crate::parser::{ParseError, Parser};
use crate::sink::Sink;
use crate::source::Source;

mod event;
mod grammar;
mod parser;
mod sink;
mod source;

#[must_use]
pub fn parse(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<ParseError>,
}

impl Parse {
    #[must_use]
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let tree = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        s.push_str(&tree[0..tree.len() - 1]);

        for error in &self.errors {
            s.push_str(&format!("\n{}", error));
        }

        s
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
}
