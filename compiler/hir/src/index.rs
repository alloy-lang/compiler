use alloy_scope::{ScopeIdx, Scopes};
use la_arena::{Arena, ArenaMap, Idx};
use maplit::btreemap;
use rustc_hash::FxHashMap;
use std::collections::BTreeMap;
use std::fmt;
use text_size::TextRange;

use crate::Name;

pub struct Index<T> {
    items: Arena<T>,
    item_ranges: ArenaMap<Idx<T>, TextRange>,
    item_names: FxHashMap<(Name, ScopeIdx), Idx<T>>,
}

struct IndexIterator<'a, T> {
    cursor: usize,
    index: &'a Index<T>,
}

impl<'a, T> Iterator for IndexIterator<'a, T> {
    type Item = (Idx<T>, &'a T, TextRange, Option<(Name, ScopeIdx)>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.index.items.len() {
            return None;
        }
        if let Some(item) = self.index.items.iter().skip(self.cursor).next() {
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

impl<'a, T> IndexIterator<'a, T> {
    pub fn new(index: &'a Index<T>) -> Self {
        Self { cursor: 0, index }
    }
}

impl<T: fmt::Debug> fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let items = self
            .iter()
            .map(|(id, item, range, name)| {
                let mut properties = btreemap! {
                    "item" => format!("{:?}", item),
                    "range" => format!("{:?}", range),
                };
                if let Some((name, scope_id)) = name {
                    properties.insert("name", format!("{name}"));
                    properties.insert("scope_id", format!("{scope_id:?}"));
                }
                (id, properties)
            })
            .collect::<BTreeMap<_, _>>();

        let mut type_name = std::any::type_name::<T>();
        if let Some(idx) = type_name.rfind(':') {
            type_name = &type_name[idx + 1..];
        }

        if items.is_empty() {
            f.debug_struct(&format!("EmptyIndex::<{type_name}>"))
                .finish()
        } else {
            f.debug_struct(&format!("Index::<{type_name}>"))
                .field("items", &items)
                .finish()
        }
    }
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

    fn iter(&self) -> IndexIterator<T> {
        IndexIterator::new(self)
    }

    pub(crate) fn insert_named(
        &mut self,
        name: Name,
        thing: T,
        thing_range: TextRange,
        scopes: &Scopes,
    ) -> Result<Idx<T>, DuplicateNameError> {
        self.check_for_name(&name, thing_range, scopes)?;

        let current_scope = scopes.current_scope();

        let id = self.insert_not_named(thing, thing_range);
        self.item_names.insert((name, current_scope), id);

        Ok(id)
    }

    pub(crate) fn add_name(
        &mut self,
        name: Name,
        id: Idx<T>,
        range: TextRange,
        scopes: &Scopes,
    ) -> Result<Idx<T>, DuplicateNameError> {
        self.check_for_name(&name, range, scopes)?;

        let current_scope = scopes.current_scope();

        self.item_names.insert((name, current_scope), id);

        Ok(id)
    }

    fn check_for_name(
        &mut self,
        name: &Name,
        range: TextRange,
        scopes: &Scopes,
    ) -> Result<(), DuplicateNameError> {
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

        scopes.push_scope("testing 1");

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
