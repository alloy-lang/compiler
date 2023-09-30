#[allow(clippy::wildcard_imports)]
use super::*;

ast_node!(ImportDef, fields: [path, targets]);

impl ImportDef {
    #[must_use]
    pub fn path(&self) -> Vec<String> {
        let (_last, rest) = Self::full_path(self);

        rest.into_iter()
            .filter_map(|child| {
                if let ImportDefChild::ImportDefSegment(segment) = child {
                    segment.name()
                } else {
                    None
                }
            })
            .collect()
    }

    #[must_use]
    pub fn targets(&self) -> Vec<String> {
        let (last, _) = Self::full_path(self);

        match last {
            ImportDefChild::ImportDefSegment(segment) => vec![segment.name().unwrap()],
            ImportDefChild::ImportDefGroup(group) => group.names(),
        }
    }

    fn full_path(&self) -> (ImportDefChild, Vec<ImportDefChild>) {
        let mut full_path = children(self);

        let last = full_path.pop().unwrap();

        (last, full_path)
    }
}

ast_union_node!(ImportDefChild, kinds: [ImportDefSegment, ImportDefGroup]);

ast_node!(ImportDefSegment, fields: [name]);

impl ImportDefSegment {
    fn name(&self) -> Option<String> {
        first_ident(self)
    }
}

ast_node!(ImportDefGroup, fields: [names]);

impl ImportDefGroup {
    fn names(&self) -> Vec<String> {
        children::<ImportDefGroup, ImportDefSegment>(self)
            .into_iter()
            .filter_map(|token| ImportDefSegment::name(&token))
            .collect()
    }
}
