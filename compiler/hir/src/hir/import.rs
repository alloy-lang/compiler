#[allow(clippy::wildcard_imports)]
use super::*;

#[allow(clippy::module_name_repetitions)]
pub type ImportIdx = Idx<Import>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    segments: Vec<Name>,
    last: Name,
}

impl Import {
    pub(super) fn new(segments: &NonEmpty<Name>) -> Self {
        let (last, path) = segments.split_last();
        Self {
            segments: path.to_vec(),
            last: last.clone(),
        }
    }

    pub fn last(&self) -> &Name {
        &self.last
    }

    pub fn segments(&self) -> &[Name] {
        &self.segments
    }

    /// Get all segments including the last one (full import path)
    pub fn all_segments(&self) -> Vec<&Name> {
        let mut all = Vec::with_capacity(self.segments.len() + 1);
        all.extend(self.segments.iter());
        all.push(&self.last);
        all
    }

    /// Convert this import to a ModuleId treating all segments as the module path
    /// For "import foo::bar::baz", this returns ModuleId("foo::bar::baz")
    pub fn to_module_id<'db>(&self, db: &'db dyn crate::HirDatabase) -> alloy_workspace::ModuleId<'db> {
        let all_segments: Vec<_> = self.all_segments().iter().map(|n| n.as_str()).collect();
        alloy_workspace::ModuleId::from_segments(db, &all_segments)
    }

    /// Split this import into (module_path, specific_export)
    /// - If there's only one segment, returns (ModuleId(segment), None)
    /// - If there are multiple segments, can return either:
    ///   - All segments as module: (ModuleId(all), None)
    ///   - All but last as module: (ModuleId(segments), Some(last))
    ///
    /// The caller should try resolving with all segments first, and if that fails,
    /// try with all-but-last as the module and last as the export
    pub fn module_and_export(&self) -> (Vec<&Name>, Option<&Name>) {
        if self.segments.is_empty() {
            // Only one segment total, must be a module name
            (vec![&self.last], None)
        } else {
            // Multiple segments - last could be either module or export
            // Return all segments; caller will try both interpretations
            (self.all_segments(), None)
        }
    }

    /// Try to split as (module_path, export_name) where export_name is the last segment
    /// Returns None if there's only one segment (can't split)
    pub fn try_split_export<'db>(&self, db: &'db dyn crate::HirDatabase) -> Option<(alloy_workspace::ModuleId<'db>, Name)> {
        if self.segments.is_empty() {
            // Only one segment, can't split
            None
        } else {
            // Use all but last as module, last as export
            let module_id = alloy_workspace::ModuleId::from_segments(
                db,
                &self.segments.iter().map(|n| n.as_str()).collect::<Vec<_>>()
            );
            Some((module_id, self.last.clone()))
        }
    }
}

pub(super) fn lower_import(ctx: &mut LoweringCtx, import: &ast::ImportDef) {
    let children = import
        .children()
        .into_iter()
        .enumerate()
        .collect::<Vec<_>>();
    let Some(((_, first), rest)) = children.split_first() else {
        unreachable!("parsing error")
    };

    match gather_all_import_segments(first, rest) {
        Ok(all_import_segments) => {
            for import_segments in all_import_segments {
                ctx.add_import(&import_segments, &import.syntax());
            }
        }
        Err(error) => {
            ctx.error(error, import.range());
        }
    }
}

fn gather_all_import_segments(
    first: &ast::ImportDefChild,
    rest: &[(usize, ast::ImportDefChild)],
) -> Result<Vec<NonEmpty<Name>>, LoweringErrorKind> {
    // add 1 because 'rest' is missing the first segment
    let num_segments = rest.len() + 1;

    let mut segments = match first {
        ast::ImportDefChild::ImportDefSegment(segment) => {
            NonEmpty::new(lower_import_def_segment(segment))
        }
        ast::ImportDefChild::ImportDefGroup(group) if 1 == num_segments => {
            let mut all = Vec::new();
            for segment in group.children() {
                all.push(NonEmpty::new(lower_import_def_segment(&segment)));
            }
            return Ok(all);
        }
        ast::ImportDefChild::ImportDefGroup(group) => {
            return Err(LoweringErrorKind::ImportGroupNotAtEnd {
                group_range: group.range(),
                position: 0,
                num_segments,
            })
        }
    };

    for (segment_num, child) in rest {
        match child {
            ast::ImportDefChild::ImportDefSegment(segment) => {
                segments.push(lower_import_def_segment(segment));
            }
            ast::ImportDefChild::ImportDefGroup(group) if segment_num + 1 == num_segments => {
                let mut all = Vec::new();
                for segment in group.children() {
                    let mut local_segments = segments.clone();
                    local_segments.push(lower_import_def_segment(&segment));
                    all.push(local_segments);
                }
                return Ok(all);
            }
            ast::ImportDefChild::ImportDefGroup(group) => {
                return Err(LoweringErrorKind::ImportGroupNotAtEnd {
                    group_range: group.range(),
                    position: *segment_num,
                    num_segments,
                })
            }
        }
    }

    Ok(vec![segments])
}

fn lower_import_def_segment(ast: &ast::ImportDefSegment) -> Name {
    match ast.name() {
        None => {
            unreachable!("parsing error")
        }
        Some(segment) => Name::new(segment.text()),
    }
}
