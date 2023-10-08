#[allow(clippy::wildcard_imports)]
use super::*;

pub(super) fn lower_import(ctx: &mut LoweringCtx, import: &ast::ImportDef) {
    // let num_segments = import.children().len() - 1;
    //
    // let mut path = vec![];
    // for (segment_num, child) in import.children().iter().enumerate() {
    //     match child {
    //         ImportDefChild::ImportDefSegment(segment) => {
    //             let segment = segment.name() else {
    //                 continue;
    //             };
    //             path.push("");
    //
    //             match segment_num {
    //                 0 => todo!("lower_import"),
    //                 s if s == num_segments => todo!("lower_import"),
    //                 _ => todo!("lower_import"),
    //             };
    //         }
    //         ImportDefChild::ImportDefGroup(group) => {
    //
    //         }
    //     }
    // }

    let mut paths = vec![];

    let mut segments = vec![];
    for (_segment_num, child) in import.children().iter().enumerate() {
        match child {
            ast::ImportDefChild::ImportDefSegment(segment) => {
                segments.push(segment.name().unwrap());
            }
            ast::ImportDefChild::ImportDefGroup(_) => {
                todo!()
            }
        }
    }

    match &segments[..] {
        [] => {}
        [segment] => {
            let path = Path::ThisModule(Name::new(segment.clone()));
            paths.push(path);
        }
        [head @ .., tail] => unsafe {
            let path = Path::OtherModule(Fqn::new(head, tail));
            paths.push(path);
        },
    }
}
