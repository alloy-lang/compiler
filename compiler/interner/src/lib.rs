pub struct Interner(lasso::Rodeo);

impl Default for Interner {
    fn default() -> Self {
        let mut interner = Self(lasso::Rodeo::default());
        // TODO: add builtin types
        // interner.intern("string");
        interner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(lasso::Spur);

impl Interner {
    pub fn intern(&mut self, s: &str) -> Key {
        Key(self.0.get_or_intern(s))
    }

    pub fn lookup(&self, key: Key) -> &str {
        self.0.resolve(&key.0)
    }
}
