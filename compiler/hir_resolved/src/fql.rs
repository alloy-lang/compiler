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
pub enum EPTrFql {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
    TypeReference(Fql<hir::TypeReference>),
}

impl EPTrFql {
    pub fn module_id(&self) -> ModuleId {
        match self {
            EPTrFql::Expression(fql) => fql.module_id,
            EPTrFql::Pattern(fql) => fql.module_id,
            EPTrFql::TypeReference(fql) => fql.module_id,
        }
    }
}

impl Into<EPTrFql> for Fql<hir::Expression> {
    fn into(self) -> EPTrFql {
        EPTrFql::Expression(self)
    }
}

impl Into<EPTrFql> for &Fql<hir::Expression> {
    fn into(self) -> EPTrFql {
        EPTrFql::Expression(self.clone())
    }
}

impl Into<EPTrFql> for Fql<hir::Pattern> {
    fn into(self) -> EPTrFql {
        EPTrFql::Pattern(self)
    }
}

impl Into<EPTrFql> for &Fql<hir::Pattern> {
    fn into(self) -> EPTrFql {
        EPTrFql::Pattern(self.clone())
    }
}

impl Into<EPTrFql> for Fql<hir::TypeReference> {
    fn into(self) -> EPTrFql {
        EPTrFql::TypeReference(self)
    }
}

impl Into<EPTrFql> for &Fql<hir::TypeReference> {
    fn into(self) -> EPTrFql {
        EPTrFql::TypeReference(self.clone())
    }
}

/// A fully qualified reference to an expression, pattern, or type_definition within a specific module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EPTdFql {
    Expression(Fql<hir::Expression>),
    Pattern(Fql<hir::Pattern>),
    TypeDefinition(Fql<hir::TypeDefinition>),
}

impl EPTdFql {
    pub fn module_id(&self) -> ModuleId {
        match self {
            EPTdFql::Expression(fql) => fql.module_id,
            EPTdFql::Pattern(fql) => fql.module_id,
            EPTdFql::TypeDefinition(fql) => fql.module_id,
        }
    }
}

impl Into<EPTdFql> for Fql<hir::Expression> {
    fn into(self) -> EPTdFql {
        EPTdFql::Expression(self)
    }
}

impl Into<EPTdFql> for &Fql<hir::Expression> {
    fn into(self) -> EPTdFql {
        EPTdFql::Expression(self.clone())
    }
}

impl Into<EPTdFql> for Fql<hir::Pattern> {
    fn into(self) -> EPTdFql {
        EPTdFql::Pattern(self)
    }
}

impl Into<EPTdFql> for &Fql<hir::Pattern> {
    fn into(self) -> EPTdFql {
        EPTdFql::Pattern(self.clone())
    }
}

impl Into<EPTdFql> for Fql<hir::TypeDefinition> {
    fn into(self) -> EPTdFql {
        EPTdFql::TypeDefinition(self)
    }
}

impl Into<EPTdFql> for &Fql<hir::TypeDefinition> {
    fn into(self) -> EPTdFql {
        EPTdFql::TypeDefinition(self.clone())
    }
}

impl Into<EPTdFql> for EPFql {
    fn into(self) -> EPTdFql {
        match self {
            EPFql::Expression(fql) => EPTdFql::Expression(fql),
            EPFql::Pattern(fql) => EPTdFql::Pattern(fql),
        }
    }
}
