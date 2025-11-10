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
