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
        println!("{}", parse.debug_tree());

        let syntax = parse.syntax();

        for error in alloy_rowan_ast::validation::validate(&syntax) {
            println!("{}", error);
        }

        let source_file = alloy_rowan_ast::SourceFile::cast(syntax).unwrap();

        dbg!(source_file
            .stmts()
            .filter_map(
                |stmt| if let alloy_rowan_ast::Stmt::VariableDef(var_def) = stmt {
                    Some(var_def.value())
                } else {
                    None
                }
            )
            .collect::<Vec<_>>());

        dbg!(alloy_rowan_hir::lower(&source_file));

        input.clear();
    }
}
