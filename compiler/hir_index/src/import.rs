#[allow(clippy::wildcard_imports)]
use super::*;
use alloy_ast::AstElement;
use non_empty_vec::NonEmpty;

// Import

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct IndexedImport {
    pub segments: Vec<SpannedName>,
    pub last: SpannedName,
}

impl IndexedImport {
    pub(super) fn equivalent(&self, other: &Self) -> bool {
        let self_s = self.segments.iter().map(|n| &n.0).collect::<Vec<_>>();
        let other_s = other.segments.iter().map(|n| &n.0).collect::<Vec<_>>();

        self.last.0 == other.last.0 && self_s == other_s
    }

    pub(crate) fn range(&self) -> TextRange {
        let first = self.segments.first().unwrap_or(&self.last);

        TextRange::new(first.1.start(), self.last.1.end())
    }
}

pub(super) fn index(ctx: &mut IndexingCtx, import: &ast::ImportDef) {
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
                let (last, path) = import_segments.split_last();
                let import = IndexedImport {
                    segments: path.to_vec(),
                    last: last.clone(),
                };
                ctx.add_import(import);
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
) -> Result<Vec<NonEmpty<SpannedName>>, IndexingErrorKind> {
    // add 1 because 'rest' is missing the first segment
    let num_segments = rest.len() + 1;

    let mut segments = match first {
        ast::ImportDefChild::ImportDefSegment(segment) => {
            let Some(segment) = lower_import_def_segment(&segment) else {
                unreachable!("parsing error");
            };
            NonEmpty::new(segment)
        }
        ast::ImportDefChild::ImportDefGroup(group) if 1 == num_segments => {
            let mut all = Vec::new();
            for segment in group.children() {
                let Some(segment) = lower_import_def_segment(&segment) else {
                    continue;
                };
                all.push(NonEmpty::new(segment));
            }
            return Ok(all);
        }
        ast::ImportDefChild::ImportDefGroup(group) => {
            return Err(IndexingErrorKind::ImportGroupNotAtEnd {
                group_range: group.range(),
                position: 0,
                num_segments,
            })
        }
    };

    for (segment_num, child) in rest {
        match child {
            ast::ImportDefChild::ImportDefSegment(segment) => {
                let Some(segment) = lower_import_def_segment(segment) else {
                    continue;
                };
                segments.push(segment);
            }
            ast::ImportDefChild::ImportDefGroup(group) if segment_num + 1 == num_segments => {
                let mut all = Vec::new();
                for segment in group.children() {
                    let mut local_segments = segments.clone();
                    let Some(segment) = lower_import_def_segment(&segment) else {
                        continue;
                    };
                    local_segments.push(segment);
                    all.push(local_segments);
                }
                return Ok(all);
            }
            ast::ImportDefChild::ImportDefGroup(group) => {
                return Err(IndexingErrorKind::ImportGroupNotAtEnd {
                    group_range: group.range(),
                    position: *segment_num,
                    num_segments,
                })
            }
        }
    }

    Ok(vec![segments])
}

fn lower_import_def_segment(ast: &ast::ImportDefSegment) -> Option<SpannedName> {
    let ident = ast.name()?;
    Some(ident.into())
}
