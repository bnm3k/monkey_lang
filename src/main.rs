use std::{env, fs};

use monkey::Monkey;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

fn main() -> eyre::Result<()> {
    let mut monkey = Monkey::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        eyre::bail!("Only takes 1 or 0 args (for the file)")
    }
    if args.len() == 2 {
        let file_path = &args[1];
        let content = fs::read_to_string(file_path)?;
        monkey.eval(&content);
        return Ok(());
    }

    // repl
    println!("Monkey lang");
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                monkey.eval(&line);
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
