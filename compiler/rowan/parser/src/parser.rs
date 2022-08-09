use std::mem;

use alloy_rowan_lexer::{Token, TokenKind};
use alloy_rowan_syntax::SyntaxKind;
pub(crate) use parse_error::ParseError;

use crate::event::Event;
use crate::grammar;
use crate::source::Source;
use crate::token_set::TokenSet;

use self::marker::Marker;

pub(crate) mod marker;

mod parse_error;

const DEFAULT_RECOVERY_SET: TokenSet = TokenSet::new([TokenKind::LetKw]);

pub(crate) struct Parser<'t, 'input> {
    source: Source<'t, 'input>,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
}

impl<'t, 'input> Parser<'t, 'input> {
    #[must_use]
    pub(crate) fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: Vec::new(),
            expected_kinds: Vec::new(),
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

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn at_set(&mut self, set: TokenSet) -> bool {
        self.peek().map_or(false, |k| set.contains(k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn expect_with_recovery(&mut self, kind: TokenKind, recovery_set: TokenSet) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery(recovery_set.union(DEFAULT_RECOVERY_SET));
        }
    }

    pub(crate) fn error(&mut self) {
        self.error_with_recovery(DEFAULT_RECOVERY_SET);
    }

    pub(crate) fn error_with_recovery(&mut self, recovery_set: TokenSet) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we’re at the end of the input we use the range of the very last token in the
            // input.
            (None, self.source.last_token_range().unwrap())
        };

        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        }));

        self.recover(recovery_set);
    }

    fn recover(&mut self, recovery_set: TokenSet) {
        if !self.at_set(recovery_set) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }
}
