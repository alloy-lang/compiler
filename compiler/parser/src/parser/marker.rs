use drop_bomb::DropBomb;

use alloy_syntax::SyntaxKind;

use crate::event::Event;
use crate::parser::Parser;

pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("Markers need to be completed"),
        }
    }

    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();

        let event_at_pos = &mut p.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        p.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }

    pub(crate) fn cancel(mut self, p: &mut Parser) {
        self.bomb.defuse();

        loop {
            match p.events.pop() {
                Some(Event::AddToken) => {
                    p.source.undo_token().unwrap();
                }
                Some(Event::Placeholder) | None => break,
                _ => {}
            }
        }
    }
}

pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(crate) fn precede(self, p: &mut Parser) -> Marker {
        let new_m = p.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = p.events[self.pos]
        {
            *forward_parent = Some(new_m.pos - self.pos);
        } else {
            unreachable!();
        }

        new_m
    }
}
