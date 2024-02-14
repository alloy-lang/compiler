use alloy_lexer::TokenKind;

// Each bit represents whether that bit’s TokenKind is in the set.
//
// This is a TokenSet containing the first and third variants of TokenKind
// (regardless of what they may be):
//
//     0000000000000101
//
// Thus, the number of TokenKind variants must not exceed
// the number of bits in TokenSet.
//
// This implementation is mostly stolen from rust-analyzer:
// https://github.com/rust-analyzer/rust-analyzer/blob/b73b321478d3b2a98d380eb79de717e01620c4e9/crates/parser/src/token_set.rs
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct TokenSet(u64);

impl TokenSet {
    pub(crate) const EMPTY: Self = Self(0);
    // pub(crate) const ALL: Self = Self(u64::MAX);

    pub(crate) const fn new<const LEN: usize>(kinds: [TokenKind; LEN]) -> Self {
        let mut value = 0;

        let mut idx = 0;
        while idx < kinds.len() {
            value |= mask(kinds[idx]);
            idx += 1;
        }

        Self(value)
    }

    pub(crate) const fn contains(self, kind: TokenKind) -> bool {
        self.0 & mask(kind) != 0
    }

    pub(crate) const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub(crate) const fn plus(self, kind: TokenKind) -> Self {
        Self(self.0 | mask(kind))
    }

    pub(crate) const fn minus(self, kind: TokenKind) -> Self {
        Self(self.0 & !mask(kind))
    }

    pub(crate) fn invert(self) -> Self {
        Self(!self.0)
    }
}

const fn mask(kind: TokenKind) -> u64 {
    1 << kind as u64
}

#[macro_export]
macro_rules! ts {
    () => (
        $crate::token_set::TokenSet::EMPTY
    );
    ($($x:expr),+ $(,)?) => (
        $crate::token_set::TokenSet::new([$($x),+])
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let set = TokenSet::new([TokenKind::LetKw, TokenKind::Integer]);

        assert!(set.contains(TokenKind::LetKw));
        assert!(set.contains(TokenKind::Integer));
        assert!(!set.contains(TokenKind::String));

        let set = set.union(TokenSet::new([TokenKind::String]));

        assert!(set.contains(TokenKind::LetKw));
        assert!(set.contains(TokenKind::Integer));
        assert!(set.contains(TokenKind::String));
        assert!(!set.contains(TokenKind::Ident));
    }

    #[test]
    fn macro_works() {
        let set1 = TokenSet::new([TokenKind::LetKw, TokenKind::Integer]);
        let set2 = ts![TokenKind::LetKw, TokenKind::Integer];

        assert!(set1.contains(TokenKind::LetKw));
        assert!(set2.contains(TokenKind::LetKw));

        assert!(set1.contains(TokenKind::Integer));
        assert!(set2.contains(TokenKind::Integer));
    }

    #[test]
    fn plus_works() {
        let set1 = TokenSet::new([TokenKind::LetKw, TokenKind::Integer]);
        let set2 = ts![TokenKind::LetKw].plus(TokenKind::Integer);

        assert!(set1.contains(TokenKind::LetKw));
        assert!(set2.contains(TokenKind::LetKw));

        assert!(set1.contains(TokenKind::Integer));
        assert!(set2.contains(TokenKind::Integer));
    }

    #[test]
    fn minus_works() {
        let set1 = TokenSet::new([TokenKind::LetKw, TokenKind::Integer, TokenKind::Ident])
            .minus(TokenKind::Ident);

        assert!(set1.contains(TokenKind::LetKw));
        assert!(set1.contains(TokenKind::Integer));
        assert!(!set1.contains(TokenKind::Ident));
    }

    #[test]
    fn invert_works() {
        let set1 = TokenSet::new([TokenKind::LetKw, TokenKind::Integer, TokenKind::Ident]);
        let set2 = set1.invert();

        assert_eq!(set1.0, !set2.0);
    }
}
