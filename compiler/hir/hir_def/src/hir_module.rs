use super::*;
use crate::index::{Index, IndexItem};
use alloy_scope::{ScopeIdx, Scopes};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Clone, PartialEq)]
pub struct HirModule {
    imports: Index<Import>,
    expressions: Index<Expression>,
    patterns: Index<Pattern>,
    type_references: Index<TypeReference>,
    type_definitions: Index<TypeDefinition>,
    type_variables: Index<TypeVariable>,
    traits: Index<Trait>,
    behaviors: Index<Behavior, (TypeIdx, TypeIdx)>,
    value_definitions: FxHashMap<ExpressionIdx, ValueDefinition>,
    scopes: Scopes,
    warnings: Vec<LoweringWarning>,
    errors: Vec<LoweringError>,
}

impl fmt::Debug for HirModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("HirModule");
        debug_struct.field("imports", &self.imports);
        debug_struct.field("expressions", &self.expressions);
        debug_struct.field("patterns", &self.patterns);
        debug_struct.field("type_references", &self.type_references);
        debug_struct.field("type_definitions", &self.type_definitions);
        if !self.type_variables.is_empty() {
            debug_struct.field("type_variables", &self.type_variables);
        }
        debug_struct.field("traits", &self.traits);
        debug_struct.field("behaviors", &self.behaviors);
        if !self.value_definitions.is_empty() {
            debug_struct.field("value_definitions", &self.value_definitions);
        }
        debug_struct.field("scopes", &self.scopes);
        debug_struct.field("warnings", &self.warnings);
        debug_struct.field("errors", &self.errors);

        debug_struct.finish()
    }
}

impl HirModule {
    pub fn warnings(&self) -> &[LoweringWarning] {
        &self.warnings
    }

    pub fn errors(&self) -> &[LoweringError] {
        &self.errors
    }

    pub(crate) fn empty() -> Self {
        Self {
            imports: Default::default(),
            expressions: Default::default(),
            patterns: Default::default(),
            type_references: Default::default(),
            type_definitions: Default::default(),
            type_variables: Default::default(),
            traits: Default::default(),
            behaviors: Default::default(),
            value_definitions: Default::default(),
            scopes: Default::default(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        imports: Index<Import>,
        expressions: Index<Expression>,
        patterns: Index<Pattern>,
        type_references: Index<TypeReference>,
        type_definitions: Index<TypeDefinition>,
        type_variables: Index<TypeVariable>,
        traits: Index<Trait>,
        behaviors: Index<Behavior, (TypeIdx, TypeIdx)>,
        value_definitions: FxHashMap<ExpressionIdx, ValueDefinition>,
        scopes: Scopes,
        warnings: Vec<LoweringWarning>,
        errors: Vec<LoweringError>,
    ) -> Self {
        Self {
            imports,
            expressions,
            patterns,
            type_references,
            type_definitions,
            type_variables,
            traits,
            behaviors,
            value_definitions,
            scopes,
            warnings,
            errors,
        }
    }

    // Iterator methods for accessing collections
    pub fn type_definitions(&'_ self) -> impl Iterator<Item = IndexItem<'_, TypeDefinition, Name>> {
        self.type_definitions.iter()
    }

    pub fn expressions(&'_ self) -> impl Iterator<Item = IndexItem<'_, Expression, Name>> {
        self.expressions.iter()
    }

    pub fn values(&'_ self) -> impl Iterator<Item = (&ExpressionIdx, &ValueDefinition)> {
        self.value_definitions.iter()
    }

    pub fn patterns(&'_ self) -> impl Iterator<Item = IndexItem<'_, Pattern, Name>> {
        self.patterns.iter()
    }

    pub fn type_references(&'_ self) -> impl Iterator<Item = IndexItem<'_, TypeReference, Name>> {
        self.type_references.iter()
    }

    pub fn imports(&'_ self) -> impl Iterator<Item = IndexItem<'_, Import, Name>> {
        self.imports.iter()
    }

    pub fn traits(&'_ self) -> impl Iterator<Item = IndexItem<'_, Trait, Name>> {
        self.traits.iter()
    }

    pub fn behaviors(
        &'_ self,
    ) -> impl Iterator<Item = IndexItem<'_, Behavior, (TypeIdx, TypeIdx)>> {
        self.behaviors.iter()
    }

    // Lookup methods by name and scope
    pub fn get_type_definition_by_name(
        &self,
        name: &Name,
    ) -> Option<(TypeDefinitionIdx, &TypeDefinition)> {
        self.type_definitions
            .get_by_scoped_name(name, Scopes::ROOT, &self.scopes)
    }

    pub fn get_type_variable_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(TypeVariableIdx, &TypeVariable)> {
        self.type_variables
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_expression_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(ExpressionIdx, &Expression)> {
        self.expressions
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_value_by_id(&'_ self, idx: &ExpressionIdx) -> Option<ValueDefinition> {
        self.value_definitions.get(idx).cloned()
    }

    pub fn get_pattern_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(PatternIdx, &Pattern)> {
        self.patterns.get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_type_reference_by_name(
        &self,
        name: &Name,
        scope: ScopeIdx,
    ) -> Option<(TypeIdx, &TypeReference)> {
        self.type_references
            .get_by_scoped_name(name, scope, &self.scopes)
    }

    pub fn get_trait_by_name(&self, name: &Name) -> Option<(TraitIdx, &Trait)> {
        self.traits
            .get_by_scoped_name(name, Scopes::ROOT, &self.scopes)
    }

    // Get methods by index
    pub fn get_type_definition(&self, idx: TypeDefinitionIdx) -> &TypeDefinition {
        self.type_definitions.get(idx)
    }

    pub fn get_type_definition_range(&self, idx: TypeDefinitionIdx) -> TextRange {
        self.type_definitions.get_range(idx)
    }

    pub fn get_type_variable(&self, idx: TypeVariableIdx) -> &TypeVariable {
        self.type_variables.get(idx)
    }

    pub fn get_expression(&self, idx: ExpressionIdx) -> &Expression {
        self.expressions.get(idx)
    }

    pub fn get_expression_range(&self, idx: ExpressionIdx) -> TextRange {
        self.expressions.get_range(idx)
    }

    pub fn get_pattern(&self, idx: PatternIdx) -> &Pattern {
        self.patterns.get(idx)
    }

    pub fn get_pattern_range(&self, idx: PatternIdx) -> TextRange {
        self.patterns.get_range(idx)
    }

    pub fn get_type_reference(&self, idx: TypeIdx) -> &TypeReference {
        self.type_references.get(idx)
    }

    pub fn get_type_reference_range(&self, idx: TypeIdx) -> TextRange {
        self.type_references.get_range(idx)
    }

    pub fn get_import(&self, idx: ImportIdx) -> &Import {
        self.imports.get(idx)
    }

    pub fn get_trait(&self, idx: TraitIdx) -> &Trait {
        self.traits.get(idx)
    }

    pub fn get_trait_range(&self, idx: TraitIdx) -> TextRange {
        self.traits.get_range(idx)
    }

    pub fn get_behavior(&self, idx: BehaviorIdx) -> &Behavior {
        self.behaviors.get(idx)
    }

    /// Find the trait that contains the given scope, if any
    /// This checks if the scope is within a trait's scope hierarchy
    pub fn find_trait_containing_scope(&self, scope: ScopeIdx) -> Option<(TraitIdx, &Trait)> {
        for (trait_idx, trait_def, _range, _name_scope) in self.traits() {
            let trait_scope = trait_def.scope();
            if self.scopes.scope_is_descendant_of(scope, trait_scope) {
                return Some((trait_idx, trait_def));
            }
        }
        None
    }

    /// Find the behavior that contains the given scope, if any
    /// This checks if the scope is within a behavior's scope hierarchy
    pub fn find_behavior_containing_scope(
        &self,
        scope: ScopeIdx,
    ) -> Option<(BehaviorIdx, &Behavior)> {
        for (behavior_idx, behavior, _range, _name_scope) in self.behaviors() {
            let behavior_scope = behavior.scope();
            if self.scopes.scope_is_descendant_of(scope, behavior_scope) {
                return Some((behavior_idx, behavior));
            }
        }
        None
    }
}
