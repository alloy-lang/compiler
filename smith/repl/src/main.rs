extern crate dirs;

use alloy_compiler::parser::parse_repl_line;
use nu_ansi_term::{Color as AnsiColor, Style};
use std::path::PathBuf;

use reedline::{
    default_emacs_keybindings, ColumnarMenu, DefaultCompleter, DefaultHinter, DefaultPrompt,
    DefaultPromptSegment, EditCommand, Emacs, ExampleHighlighter, FileBackedHistory, KeyCode,
    KeyModifiers, Reedline, ReedlineEvent, ReedlineMenu, Signal,
};

fn main() {
    reedline_repl();
}

fn reedline_repl() {
    let prompt = DefaultPrompt::new(
        DefaultPromptSegment::WorkingDirectory,
        DefaultPromptSegment::CurrentDateTime,
    );

    let commands = vec!["let".into(), "=".into()];

    let completer = Box::new(DefaultCompleter::new_with_wordlen(commands.clone(), 2));
    // Use the interactive menu to select options from the completer
    let completion_menu = Box::new(ColumnarMenu::default().with_name("completion_menu"));
    // Set up the required keybindings
    let mut keybindings = default_emacs_keybindings();
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );

    let edit_mode = Box::new(Emacs::new(keybindings));

    let history_file = get_history_file_path();
    let history = Box::new(
        FileBackedHistory::with_file(usize::MAX - 1, history_file)
            .expect("Error configuring history with file"),
    );

    let hinter = Box::new(
        DefaultHinter::default().with_style(Style::new().italic().fg(AnsiColor::LightGray)),
    );

    let mut line_editor = Reedline::create()
        .with_history(history)
        .with_highlighter(Box::new(ExampleHighlighter::new(commands)))
        .with_completer(completer)
        .with_partial_completions(true)
        .with_hinter(hinter)
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode);

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => match buffer.as_str() {
                ":history" => {
                    println!("History:");
                    line_editor
                        .print_history_session()
                        .expect("Failed to print history");
                }
                ":help" => {
                    println!("Commands:");
                    break;
                }
                ":exit" => {
                    println!("Goodbye!");
                    break;
                }
                _ => {
                    let parse = parse_repl_line(buffer.as_str());
                    println!("{}", parse.debug_tree());

                    let syntax = parse.syntax();

                    for error in alloy_ast::validation::validate(&syntax) {
                        println!("{}", error);
                    }

                    let source_file = alloy_ast::source_file(syntax).unwrap();

                    let ast_statements = source_file.statements();
                    let hir_statements = alloy_hir::lower_repl_line(&source_file);

                    dbg!(ast_statements);
                    dbg!(hir_statements);
                }
            },
            Ok(Signal::CtrlD) => {
                println!("To exit, type \":exit\"");
            }
            Ok(Signal::CtrlC) => {
                line_editor.run_edit_commands(&[EditCommand::Clear]);
            }
            x => {
                println!("Event: {:?}", x);
            }
        }
    }
}

fn get_history_file_path() -> PathBuf {
    let history_file = {
        let mut home_dir = dirs::home_dir().expect("Failed to get home directory");
        home_dir.push(".alloy");
        std::fs::create_dir_all(home_dir.as_path()).expect("Failed to create alloy directory");

        home_dir.push("smith_history");
        home_dir
    };
    history_file
}
