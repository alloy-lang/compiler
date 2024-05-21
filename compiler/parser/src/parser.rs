use std::mem;

use alloy_lexer::{Token, TokenKind};
use alloy_syntax::SyntaxKind;

use crate::event::Event;
use crate::parser::marker::CompletedMarker;
use crate::parser::parse_error::ParseErrorKind;
use crate::source::Source;
use crate::token_set::TokenSet;
use crate::{grammar, ts};
pub use parse_error::ParseError;
pub(crate) use parse_error::ParseErrorContext;

use self::marker::Marker;

pub(crate) mod marker;

mod parse_error;

pub(crate) const DEFAULT_RECOVERY_SET: TokenSet = ts![
    TokenKind::LetKw,
    TokenKind::ImportKw,
    TokenKind::BehaviorKw,
    TokenKind::TraitKw,
    TokenKind::TypeOfKw,
    TokenKind::TypedefKw,
];

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
    pub(crate) fn parse_source_file(mut self) -> Vec<Event> {
        grammar::source_file(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.source.peek_nth_kind(0) == Some(kind)
    }

    pub(crate) fn maybe_at(&mut self, kind: TokenKind) -> bool {
        self.source.peek_nth_kind(0) == Some(kind)
    }

    pub(crate) fn maybe_at_nth(&mut self, kind: TokenKind, skip: usize) -> bool {
        self.peek_nth(skip) == Some(kind)
    }

    pub(crate) fn at_set(&mut self, set: TokenSet) -> bool {
        self.source
            .peek_nth_kind(0)
            .map_or(false, |k| set.contains(k))
    }

    pub(crate) fn at_top_level_token_or_set(&mut self, set: TokenSet) -> bool {
        self.at_set(DEFAULT_RECOVERY_SET.union(set)) || self.at_eof()
    }

    pub(crate) fn at_top_level_token_or_not_set(&mut self, set: TokenSet) -> bool {
        self.at_set(DEFAULT_RECOVERY_SET.union(set.invert())) || self.at_eof()
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.source.peek_nth_kind(0).is_none()
    }

    fn peek_nth(&mut self, skip: usize) -> Option<TokenKind> {
        self.source.peek_nth_kind(skip)
    }

    pub(crate) fn bump_any(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn bump(&mut self, kind: TokenKind) {
        assert!(self.at(kind));
        self.bump_any();
    }

    pub(crate) fn expect(&mut self, kind: TokenKind, context: ParseErrorContext) {
        if self.at(kind) {
            self.bump(kind);
        } else {
            self.error(context);
        }
    }

    pub(crate) fn expect_only(&mut self, kind: TokenKind, context: ParseErrorContext) {
        self.expected_kinds.clear();
        self.expect(kind, context);
    }

    pub(crate) fn expect_with_recovery(
        &mut self,
        kind: TokenKind,
        context: ParseErrorContext,
        recovery_set: TokenSet,
    ) {
        if self.at(kind) {
            self.bump(kind);
        } else {
            self.error_with_recovery(context, recovery_set);
        }
    }

    fn error(&mut self, context: ParseErrorContext) -> Option<CompletedMarker> {
        self.error_with_recovery(context, ts![])
    }

    pub(crate) fn error_with_recovery(
        &mut self,
        context: ParseErrorContext,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        self.error_with_recovery_no_default(context, recovery_set.union(DEFAULT_RECOVERY_SET))
    }

    fn error_with_recovery_no_default(
        &mut self,
        context: ParseErrorContext,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        let last_token_range = self.source.last_token_range().unwrap_or_default();

        let current_token = self.source.peek_nth_token(0);
        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we’re at the end of the input we use the range of the very last token in the
            // input.
            (None, last_token_range)
        };

        let kind = match found {
            None => ParseErrorKind::Missing {
                offset: range.end(),
            },
            Some(kind) => {
                if self.at_set(recovery_set) {
                    ParseErrorKind::Missing {
                        offset: range.start(),
                    }
                } else {
                    ParseErrorKind::Unexpected { found: kind, range }
                }
            }
        };

        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            kind,
            context,
        }));

        if !self.at_set(recovery_set) && !self.at_eof() {
            let m = self.start();
            self.bump_any();
            return Some(m.complete(self, SyntaxKind::Error));
        };
        None
    }
}
