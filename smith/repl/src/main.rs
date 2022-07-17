use alloy_rowan_parser::parse;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();

    loop {
        write!(stdout, "→ ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let parse = parse(&input);

        let root = alloy_rowan_ast::Root::cast(parse.syntax()).unwrap();

        dbg!(root
            .stmts()
            .filter_map(
                |stmt| if let alloy_rowan_ast::Stmt::VariableDef(var_def) = stmt {
                    Some(var_def.value())
                } else {
                    None
                }
            )
            .collect::<Vec<_>>());

        dbg!(alloy_rowan_hir::lower(root));

        input.clear();
    }
}
