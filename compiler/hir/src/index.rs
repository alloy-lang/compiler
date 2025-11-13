use alloy_scope::{ScopeIdx, Scopes};
use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;
use std::collections::BTreeMap;
use std::fmt;
use std::hash::Hash;
use text_size::TextRange;

use crate::Name;

#[derive(PartialEq)]
pub struct Index<T, N: Eq + Hash = Name> {
    items: Arena<T>,
    item_ranges: ArenaMap<Idx<T>, TextRange>,
    item_names: FxHashMap<(N, ScopeIdx), Idx<T>>,
}

pub(crate) struct IndexIterator<'a, T, N: Eq + Hash> {
    cursor: usize,
    index: &'a Index<T, N>,
}

pub type IndexItem<'a, T, N> = (Idx<T>, &'a T, TextRange, Option<(N, ScopeIdx)>);

impl<'a, T, N: Eq + Hash + Clone> Iterator for IndexIterator<'a, T, N> {
    type Item = IndexItem<'a, T, N>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.index.items.len() {
            return None;
        }
        if let Some(item) = self.index.items.iter().nth(self.cursor) {
            let range = self.index.item_ranges[item.0];
            let name = self.index.item_names.iter().find_map(|(key, value)| {
                if *value == item.0 {
                    Some(key.clone())
                } else {
                    None
                }
            });

            self.cursor += 1;
            Some((item.0, item.1, range, name))
        } else {
            None
        }
    }
}

impl<'a, T, N: Eq + Hash> IndexIterator<'a, T, N> {
    pub fn new(index: &'a Index<T, N>) -> Self {
        Self { cursor: 0, index }
    }
}

impl<T: fmt::Debug, N: Eq + Hash + Clone + fmt::Debug> fmt::Debug for Index<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut type_name = std::any::type_name::<T>();
        if let Some(idx) = type_name.rfind(':') {
            type_name = &type_name[idx + 1..];
        }

        if self.is_empty() {
            return f
                .debug_struct(&format!("EmptyIndex::<{type_name}>"))
                .finish();
        }

        let mut debug_struct = f.debug_struct(&format!("Index::<{type_name}>"));
        for (id, item, range, name_info) in self.iter() {
            let mut properties: BTreeMap<&str, &dyn fmt::Debug> = BTreeMap::new();
            properties.insert("item", item);
            properties.insert("range", &range);

            if let Some((name, scope_id)) = name_info {
                properties.insert("name", &name);
                properties.insert("scope_id", &scope_id);
                // this is duplicated because borrowing is a little funky in an if statement
                debug_struct.field(&format!("{id:?}"), &properties);
            } else {
                debug_struct.field(&format!("{id:?}"), &properties);
            }
        }

        debug_struct.finish()
    }
}

#[derive(Debug)]
pub(crate) struct DuplicateNameError<N> {
    pub(crate) name: N,
    pub(crate) first: TextRange,
    pub(crate) second: TextRange,
}

impl<T, N: Eq + Hash + Clone + fmt::Debug> Index<T, N> {
    pub(crate) fn new() -> Self {
        Self {
            items: Arena::new(),
            item_ranges: ArenaMap::new(),
            item_names: FxHashMap::default(),
        }
    }

    fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub(crate) fn iter(&self) -> IndexIterator<T, N> {
        IndexIterator::new(self)
    }

    pub(crate) fn insert_named(
        &mut self,
        name: N,
        thing: T,
        thing_range: TextRange,
        scopes: &Scopes,
    ) -> Result<Idx<T>, DuplicateNameError<N>> {
        self.check_for_name(&name, thing_range, scopes)?;

        let current_scope = scopes.current_scope();

        let id = self.insert_not_named(thing, thing_range);
        self.item_names.insert((name, current_scope), id);

        Ok(id)
    }

    pub(crate) fn add_name(
        &mut self,
        name: N,
        id: Idx<T>,
        range: TextRange,
        scopes: &Scopes,
    ) -> Result<Idx<T>, DuplicateNameError<N>> {
        self.check_for_name(&name, range, scopes)?;

        let current_scope = scopes.current_scope();

        self.item_names.insert((name, current_scope), id);

        Ok(id)
    }

    fn check_for_name(
        &mut self,
        name: &N,
        range: TextRange,
        scopes: &Scopes,
    ) -> Result<(), DuplicateNameError<N>> {
        let current_scope = scopes.current_scope();

        if let Some(id) = self.item_names.get(&(name.clone(), current_scope)).copied() {
            let first = self.item_ranges[id];
            let second = range;
            let err = DuplicateNameError {
                name: name.clone(),
                first,
                second,
            };
            return Err(err);
        }
        Ok(())
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

    pub fn get_id(&self, name: &N, scopes: &Scopes) -> Option<Idx<T>> {
        scopes
            .iter()
            .find_map(|scope| self.item_names.get(&(name.clone(), scope)))
            .copied()
    }

    fn get_id_scoped(&self, name: &N, scope: ScopeIdx) -> Option<Idx<T>> {
        self.item_names.get(&(name.clone(), scope)).copied()
    }

    #[cfg(test)]
    pub fn get_by_name(&self, name: &N, scopes: &Scopes) -> Option<(Idx<T>, &T)> {
        self.get_id(name, scopes).map(|id| (id, self.get(id)))
    }

    pub fn get_by_scoped_name(&self, name: &N, scope: ScopeIdx) -> Option<(Idx<T>, &T)> {
        self.get_id_scoped(name, scope).map(|id| (id, self.get(id)))
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
        assert_eq!(index.get_by_name(&name, &scopes), Some((idx, &thing)));
    }

    #[test]
    fn index_allows_for_retrieval_when_lookup_has_parentheses() {
        let scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let insert_name = Name::new("hello");
        let lookup_name = Name::new("(hello)");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        let idx = index
            .insert_named(insert_name, thing, range, &scopes)
            .expect("testing");

        assert_eq!(index.get(idx), &thing);
        assert_eq!(index.get_range(idx), range);
        assert_eq!(index.get_id(&lookup_name, &scopes), Some(idx));
        assert_eq!(
            index.get_by_name(&lookup_name, &scopes),
            Some((idx, &thing))
        );
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

        scopes.push_scope("testing 1");

        assert_eq!(index.get_id(&name, &scopes), Some(idx));
        assert_eq!(index.get_by_name(&name, &scopes), Some((idx, &thing)));
    }

    #[test]
    fn retrieval_cannot_check_child_scopes() {
        let mut scopes = Scopes::default();
        let mut index = Index::new();

        let thing = 1;
        let name = Name::new("hello");
        let range = TextRange::new(TextSize::from(5), TextSize::from(6));

        scopes.push_scope("testing 1");

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

        scopes.push_scope("testing 1");

        let idx_2 = index
            .insert_named(name.clone(), thing, range, &scopes)
            .expect("testing");

        assert_ne!(idx_1, idx_2);
    }
}
