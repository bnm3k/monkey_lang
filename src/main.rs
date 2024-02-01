use monkey::evaluator::eval_program;
use monkey::object::Object;
use monkey::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

fn main() -> eyre::Result<()> {
    println!("Monkey lang");
    let mut rl = DefaultEditor::new()?;
    'outer: loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                // parse
                let program = match Parser::parse(&line) {
                    Ok(program) => program,
                    Err(errs) => {
                        for err in errs {
                            println!("Error: {}", err);
                        }
                        continue 'outer;
                    }
                };
                // eval
                let res = eval_program(program);
                println!("{}", res.inspect());
                // match res {
                //     Object::Null => continue 'outer,
                //     v @ _ => println!("{}", v.inspect()),
                // }
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
