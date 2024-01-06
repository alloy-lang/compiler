use la_arena::{Arena, Idx, RawIdx};
pub type ScopeIdx = Idx<Scope>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Scope {
    parent: ScopeIdx,
}

#[derive(Debug)]
pub struct Scopes {
    current_scope: ScopeIdx,
    scopes: Arena<Scope>,
}

impl Default for Scopes {
    fn default() -> Self {
        Scopes::new()
    }
}

impl Scopes {
    pub const ROOT: ScopeIdx = Idx::from_raw(RawIdx::from_u32(0));

    #[must_use]
    pub fn new() -> Self {
        let mut scopes = Arena::new();

        let root_scope_id = scopes.alloc(Scope {
            parent: Scopes::ROOT,
        });

        Self {
            current_scope: root_scope_id,
            scopes,
        }
    }

    #[must_use]
    pub fn current_scope(&self) -> ScopeIdx {
        self.current_scope
    }

    #[must_use]
    pub fn parent_scope(&self, scope_id: ScopeIdx) -> ScopeIdx {
        self.scopes[scope_id].parent
    }

    pub fn push_scope(&mut self) -> ScopeIdx {
        let new_scope_id = self.scopes.alloc(Scope {
            parent: self.current_scope,
        });

        self.current_scope = new_scope_id;

        new_scope_id
    }

    pub fn pop_scope(&mut self) -> ScopeIdx {
        let current_scope = self.current_scope;

        self.current_scope = self.scopes[current_scope].parent;

        current_scope
    }

    #[must_use]
    pub fn iter(&self) -> ScopesIterator {
        ScopesIterator::new(self)
    }
}

pub struct ScopesIterator<'a> {
    scopes: &'a Scopes,
    current_scope: ScopeIdx,
    complete: bool,
}

impl<'a> ScopesIterator<'a> {
    #[must_use]
    pub fn new(scopes: &'a Scopes) -> Self {
        ScopesIterator {
            scopes,
            current_scope: scopes.current_scope(),
            complete: false,
        }
    }
}

impl<'a> Iterator for ScopesIterator<'a> {
    type Item = ScopeIdx;

    fn next(&mut self) -> Option<Self::Item> {
        if self.complete {
            return None;
        }

        let current_scope = self.current_scope;
        let parent_scope = self.scopes.parent_scope(current_scope);
        if current_scope == parent_scope {
            self.complete = true;
        }
        self.current_scope = parent_scope;

        Some(current_scope)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_current_scope_is_root() {
        let scopes = Scopes::new();

        assert_eq!(Scopes::ROOT, scopes.current_scope);
    }

    #[test]
    fn push_scope() {
        let mut scopes = Scopes::new();

        let new_scope_id_1 = scopes.push_scope();

        assert_eq!(new_scope_id_1, scopes.current_scope);

        let _new_scope_id_2 = scopes.push_scope();
        let new_scope_id_3 = scopes.push_scope();

        assert_eq!(new_scope_id_3, scopes.current_scope);
    }

    #[test]
    #[should_panic(expected = "")] // empty error message
    fn pop_scope_at_bottom_panics() {
        let mut scopes = Scopes::new();

        let _ = scopes.pop_scope();

        unreachable!("")
    }

    #[test]
    fn pop_scope() {
        let mut scopes = Scopes::new();

        let _ = scopes.push_scope();
        let _ = scopes.pop_scope();

        assert_eq!(Scopes::ROOT, scopes.current_scope);
    }

    #[test]
    fn iter_no_new_scopes() {
        let scopes = Scopes::new();

        assert_eq!(
            vec![
                Idx::from_raw(RawIdx::from_u32(0)),
            ],
            scopes.iter().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn iter() {
        let mut scopes = Scopes::new();

        scopes.push_scope();
        scopes.push_scope();
        scopes.pop_scope();
        scopes.push_scope();

        assert_eq!(
            vec![
                Idx::from_raw(RawIdx::from_u32(3)),
                Idx::from_raw(RawIdx::from_u32(1)),
                Idx::from_raw(RawIdx::from_u32(0)),
            ],
            scopes.iter().collect::<Vec<_>>(),
        );
    }
}
