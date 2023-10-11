#[allow(clippy::wildcard_imports)]
use super::*;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Import {
    segments: Vec<Name>,
    last: Name,
}

impl Import {
    pub(crate) fn new(segments: NonEmpty<Name>) -> Self {
        let (last, path) = segments.split_last();
        Self {
            segments: path.to_vec(),
            last: last.clone(),
        }
    }

    pub fn last(&self) -> &Name {
        &self.last
    }

    pub fn path(&self) -> &[Name] {
        &self.segments
    }
}

pub(super) fn lower_import(ctx: &mut LoweringCtx, import: &ast::ImportDef) {
    let children = import
        .children()
        .into_iter()
        .enumerate()
        .collect::<Vec<_>>();
    let num_segments = children.len();
    let Some(((_, first), rest)) = children.split_first() else {
        // todo!("validation");
        return;
    };
    let mut segments = match first {
        ast::ImportDefChild::ImportDefSegment(segment) => {
            let Some(segment) = lower_import_def_segment(segment) else {
                return;
            };

            NonEmpty::new(segment)
        }
        ast::ImportDefChild::ImportDefGroup(_) => {
            // todo!("validation");
            return;
        }
    };

    for (segment_num, child) in rest {
        match child {
            ast::ImportDefChild::ImportDefSegment(segment) => {
                let Some(segment) = lower_import_def_segment(segment) else {
                    return;
                };

                segments.push(segment);
            }
            ast::ImportDefChild::ImportDefGroup(group) if segment_num + 1 == num_segments => {
                for segment in group.children() {
                    let Some(segment) = lower_import_def_segment(&segment) else {
                        return;
                    };

                    segments.push(segment);
                }
            }
            ast::ImportDefChild::ImportDefGroup(_) => {
                // todo!("validation");
                return;
            }
        }
    }

    ctx.add_import(segments);
}

fn lower_import_def_segment(ast: &ast::ImportDefSegment) -> Option<Name> {
    match ast.name() {
        None => {
            // todo!("validation");
            return None;
        }
        Some(segment) => Some(Name::new(segment)),
    }
}
