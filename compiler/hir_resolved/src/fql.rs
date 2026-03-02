use alloy_hir as hir;
use alloy_workspace::ModuleId;
use la_arena::Idx;
use std::hash::Hash;
use text_size::TextRange;

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

impl Fql<hir::Trait> {
    pub fn trait_name(&self, db: &dyn hir::HirDatabase) -> hir::Name {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_trait(self.local_id).name().clone()
    }

    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_trait_range(self.local_id)
    }
}

impl Fql<hir::TypeDefinition> {
    pub fn type_def_name(&self, db: &dyn hir::HirDatabase) -> hir::Name {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_type_definition(self.local_id).name.clone()
    }

    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_type_definition_range(self.local_id)
    }
}

impl Fql<hir::Expression> {
    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_expression_range(self.local_id)
    }
}

impl Fql<hir::Pattern> {
    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_pattern_range(self.local_id)
    }
}

impl Fql<hir::TypeReference> {
    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        let (hir_module, _) = hir::lower_file(db, self.module_id);
        hir_module.get_type_reference_range(self.local_id)
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

impl From<Fql<hir::Expression>> for EPFql {
    fn from(val: Fql<hir::Expression>) -> Self {
        EPFql::Expression(val)
    }
}

impl From<&Fql<hir::Expression>> for EPFql {
    fn from(val: &Fql<hir::Expression>) -> Self {
        EPFql::Expression(val.clone())
    }
}

impl From<Fql<hir::Pattern>> for EPFql {
    fn from(val: Fql<hir::Pattern>) -> Self {
        EPFql::Pattern(val)
    }
}

impl From<&Fql<hir::Pattern>> for EPFql {
    fn from(val: &Fql<hir::Pattern>) -> Self {
        EPFql::Pattern(val.clone())
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

    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        match self {
            EPTrFql::Expression(fql) => fql.text_range(db),
            EPTrFql::Pattern(fql) => fql.text_range(db),
            EPTrFql::TypeReference(fql) => fql.text_range(db),
        }
    }
}

impl From<Fql<hir::Expression>> for EPTrFql {
    fn from(val: Fql<hir::Expression>) -> Self {
        EPTrFql::Expression(val)
    }
}

impl From<&Fql<hir::Expression>> for EPTrFql {
    fn from(val: &Fql<hir::Expression>) -> Self {
        EPTrFql::Expression(val.clone())
    }
}

impl From<Fql<hir::Pattern>> for EPTrFql {
    fn from(val: Fql<hir::Pattern>) -> Self {
        EPTrFql::Pattern(val)
    }
}

impl From<&Fql<hir::Pattern>> for EPTrFql {
    fn from(val: &Fql<hir::Pattern>) -> Self {
        EPTrFql::Pattern(val.clone())
    }
}

impl From<Fql<hir::TypeReference>> for EPTrFql {
    fn from(val: Fql<hir::TypeReference>) -> Self {
        EPTrFql::TypeReference(val)
    }
}

impl From<&Fql<hir::TypeReference>> for EPTrFql {
    fn from(val: &Fql<hir::TypeReference>) -> Self {
        EPTrFql::TypeReference(val.clone())
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

    pub fn text_range(&self, db: &dyn hir::HirDatabase) -> TextRange {
        match self {
            EPTdFql::Expression(fql) => fql.text_range(db),
            EPTdFql::Pattern(fql) => fql.text_range(db),
            EPTdFql::TypeDefinition(fql) => fql.text_range(db),
        }
    }
}

impl From<Fql<hir::Expression>> for EPTdFql {
    fn from(val: Fql<hir::Expression>) -> Self {
        EPTdFql::Expression(val)
    }
}

impl From<&Fql<hir::Expression>> for EPTdFql {
    fn from(val: &Fql<hir::Expression>) -> Self {
        EPTdFql::Expression(val.clone())
    }
}

impl From<Fql<hir::Pattern>> for EPTdFql {
    fn from(val: Fql<hir::Pattern>) -> Self {
        EPTdFql::Pattern(val)
    }
}

impl From<&Fql<hir::Pattern>> for EPTdFql {
    fn from(val: &Fql<hir::Pattern>) -> Self {
        EPTdFql::Pattern(val.clone())
    }
}

impl From<Fql<hir::TypeDefinition>> for EPTdFql {
    fn from(val: Fql<hir::TypeDefinition>) -> Self {
        EPTdFql::TypeDefinition(val)
    }
}

impl From<&Fql<hir::TypeDefinition>> for EPTdFql {
    fn from(val: &Fql<hir::TypeDefinition>) -> Self {
        EPTdFql::TypeDefinition(val.clone())
    }
}

impl From<EPFql> for EPTdFql {
    fn from(val: EPFql) -> Self {
        match val {
            EPFql::Expression(fql) => EPTdFql::Expression(fql),
            EPFql::Pattern(fql) => EPTdFql::Pattern(fql),
        }
    }
}
