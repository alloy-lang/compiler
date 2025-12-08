use alloy_hir as hir;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use std::hash::Hash;

#[derive(Debug, Clone, Copy)]
pub struct Fql<T> {
    pub module_id: ModuleId,
    pub local_id: Idx<T>,
}

impl<T> Fql<T> {
    pub fn new(module_id: ModuleId, local_id: impl Into<Idx<T>>) -> Self {
        Self {
            module_id,
            local_id: local_id.into(),
        }
    }
}

impl<T> PartialEq for Fql<T> {
    fn eq(&self, other: &Self) -> bool {
        self.module_id == other.module_id && self.local_id == other.local_id
    }
}

impl<T> Eq for Fql<T> {}

impl<T> Hash for Fql<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module_id.hash(state);
        self.local_id.hash(state);
    }
}

/// A fully qualified reference to an expression or pattern within a specific module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EPFql {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
}

impl EPFql {
    pub fn module_id(&self) -> ModuleId {
        match self {
            EPFql::Expression(fql) => fql.module_id,
            EPFql::Pattern(fql) => fql.module_id,
        }
    }
}

impl Into<EPFql> for Fql<hir::Expression> {
    fn into(self) -> EPFql {
        EPFql::Expression(self)
    }
}

impl Into<EPFql> for &Fql<hir::Expression> {
    fn into(self) -> EPFql {
        EPFql::Expression(self.clone())
    }
}

impl Into<EPFql> for Fql<hir::Pattern> {
    fn into(self) -> EPFql {
        EPFql::Pattern(self)
    }
}

impl Into<EPFql> for &Fql<hir::Pattern> {
    fn into(self) -> EPFql {
        EPFql::Pattern(self.clone())
    }
}

/// A fully qualified reference to an expression, pattern, or type_reference within a specific module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EPTFql {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
    TypeReference(Fql<hir::TypeReference>),
}

impl EPTFql {
    pub fn module_id(&self) -> ModuleId {
        match self {
            EPTFql::Expression(fql) => fql.module_id,
            EPTFql::Pattern(fql) => fql.module_id,
            EPTFql::TypeReference(fql) => fql.module_id,
        }
    }
}

impl Into<EPTFql> for Fql<hir::Expression> {
    fn into(self) -> EPTFql {
        EPTFql::Expression(self)
    }
}

impl Into<EPTFql> for &Fql<hir::Expression> {
    fn into(self) -> EPTFql {
        EPTFql::Expression(self.clone())
    }
}

impl Into<EPTFql> for Fql<hir::Pattern> {
    fn into(self) -> EPTFql {
        EPTFql::Pattern(self)
    }
}

impl Into<EPTFql> for &Fql<hir::Pattern> {
    fn into(self) -> EPTFql {
        EPTFql::Pattern(self.clone())
    }
}

impl Into<EPTFql> for Fql<hir::TypeReference> {
    fn into(self) -> EPTFql {
        EPTFql::TypeReference(self)
    }
}

impl Into<EPTFql> for &Fql<hir::TypeReference> {
    fn into(self) -> EPTFql {
        EPTFql::TypeReference(self.clone())
    }
}
