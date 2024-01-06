use alloy_scope::{ScopeIdx, Scopes};
use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;
use text_size::TextRange;

use crate::Name;

#[derive(Debug)]
pub struct Index<T> {
    items: Arena<T>,
    item_ranges: ArenaMap<Idx<T>, TextRange>,
    item_names: FxHashMap<(Name, ScopeIdx), Idx<T>>,
}

#[derive(Debug)]
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
        scopes: &Scopes,
    ) -> Result<Idx<T>, DuplicateNameError> {
        let current_scope = scopes.current_scope();

        if let Some(id) = self.item_names.get(&(name.clone(), current_scope)).copied() {
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
        self.item_names.insert((name, current_scope), id);

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

    pub fn get_id(&self, name: &Name, scopes: &Scopes) -> Option<Idx<T>> {
        scopes
            .iter()
            .find_map(|scope| self.item_names.get(&(name.clone(), scope)))
            .copied()
    }

    pub fn get_by_name(&self, name: &Name, scopes: &Scopes) -> Option<&T> {
        self.get_id(name, scopes).map(|id| self.get(id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_scope::Scopes;
    use text_size::TextSize;

    #[test]
    fn index_allows_for_retrieval() {
        let scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let name = Name::new("hello");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        let idx = index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        assert_eq!(index.get(idx), &thing);
        assert_eq!(index.get_range(idx), range);
        assert_eq!(index.get_id(&name, &scopes), Some(idx));
        assert_eq!(index.get_by_name(&name, &scopes), Some(&thing));
    }

    #[test]
    fn retrieval_checks_parent_scopes() {
        let mut scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let name = Name::new("hello");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        let idx = index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        scopes.push_scope();

        assert_eq!(index.get_id(&name, &scopes), Some(idx));
        assert_eq!(index.get_by_name(&name, &scopes), Some(&thing));
    }

    #[test]
    fn retrieval_cannot_check_child_scopes() {
        let mut scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let name = Name::new("hello");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        scopes.push_scope();

        index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        scopes.pop_scope();

        assert_eq!(index.get_id(&name, &scopes), None);
        assert_eq!(index.get_by_name(&name, &scopes), None);
    }

    #[test]
    fn insertion_is_unique_per_scope() {
        let mut scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let name = Name::new("hello");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        let idx_1 = index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        scopes.push_scope();

        let idx_2 = index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        assert_ne!(idx_1, idx_2);
    }
}
