use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;
use text_size::TextRange;

use crate::Name;

#[derive(Debug)]
pub struct Index<T> {
    items: Arena<T>,
    item_ranges: ArenaMap<Idx<T>, TextRange>,
    item_names: FxHashMap<Name, Idx<T>>,
}

pub(crate) struct DuplicateNameError {
    pub(crate) name: Name,
    pub(crate) first: TextRange,
    pub(crate) second: TextRange,
}

impl<T> Index<T> {
    pub(crate) fn new() -> Self {
        Self {
            items: Arena::new(),
            item_ranges: ArenaMap::new(),
            item_names: FxHashMap::default(),
        }
    }

    pub(crate) fn insert_named(
        &mut self,
        name: Name,
        thing: T,
        range: TextRange,
    ) -> Result<Idx<T>, DuplicateNameError> {
        if let Some(id) = self.item_names.get(&name).copied() {
            let first = self.item_ranges[id];
            let second = range;
            let err = DuplicateNameError {
                name,
                first,
                second,
            };
            return Err(err);
        }

        let id = self.insert_not_named(thing, range);
        self.item_names.insert(name, id);

        Ok(id)
    }

    pub fn insert_not_named(&mut self, thing: T, range: TextRange) -> Idx<T> {
        let id = self.items.alloc(thing);
        self.item_ranges.insert(id, range);
        id
    }

    pub fn get(&self, id: Idx<T>) -> &T {
        &self.items[id]
    }

    pub fn get_range(&self, id: Idx<T>) -> TextRange {
        self.item_ranges[id]
    }

    pub fn get_id(&self, name: &Name) -> Option<Idx<T>> {
        self.item_names.get(name).copied()
    }

    pub fn get_by_name(&self, name: &Name) -> Option<&T> {
        self.item_names.get(name).copied().map(|id| self.get(id))
    }
}
