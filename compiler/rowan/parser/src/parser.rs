use self::marker::Marker;
use crate::event::Event;
use crate::grammar;
use crate::source::Source;
use alloy_rowan_syntax::SyntaxKind;

pub(crate) mod marker;

pub(crate) struct Parser<'t, 'input> {
    source: Source<'t, 'input>,
    events: Vec<Event>,
}

impl<'t, 'input> Parser<'t, 'input> {
    #[must_use]
    pub(crate) fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: Vec::new(),
        }
    }

    #[must_use]
    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
    }

    pub(crate) fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn bump(&mut self) {
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]]);
    }

    #[test]
    fn parse_whitespace() {
        check(
            "   ",
            expect![[r#"
Root@0..3
  Whitespace@0..3 "   ""#]],
        );
    }

    #[test]
    fn parse_comment() {
        check(
            "# hello!",
            expect![[r##"
Root@0..8
  Comment@0..8 "# hello!""##]],
        );
    }
}
