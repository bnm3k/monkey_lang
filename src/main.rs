use monkey::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

fn main() -> eyre::Result<()> {
    println!("Monkey lang");
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                let parser = Parser::new(&line);
                match parser.parse_program() {
                    Ok(_) => {}
                    Err(errs) => {
                        for err in errs {
                            println!("Error: {}", err);
                        }
                    }
                }
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
