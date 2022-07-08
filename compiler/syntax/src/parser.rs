mod event;
mod expr;
mod sink;

use rowan::GreenNode;
use std::iter::Peekable;

use crate::lexer::{Lexer, SyntaxKind};
use crate::parser::event::Event;
use crate::syntax::SyntaxNode;
use expr::expr;
use sink::Sink;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    events: Vec<Event>,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            events: Vec::new(),
        }
    }

    #[must_use]
    pub fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Root);
        expr(&mut self);
        self.finish_node();

        let sink = Sink::new(self.events);

        Parse {
            green_node: sink.finish(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn checkpoint(&self) -> usize {
        self.events.len()
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(kind, _)| *kind)
    }

    fn bump(&mut self) {
        let (kind, text) = self.lexer.next().unwrap();

        self.events.push(Event::AddToken {
            kind,
            text: text.into(),
        });
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    #[must_use]
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        formatted[0..formatted.len() - 1].to_string()
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = Parser::new(input).parse();
    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]]);
    }
}
