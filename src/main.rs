use monkey::token::TokenType;
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
                let lexer = monkey::lexer::Lexer::new(&line);
                // let tokens = lexer.collect::<Vec<_>>();
                for token in lexer.take_while(|t| t.token_type != TokenType::EOF) {
                    println!("{:?}", token);
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
