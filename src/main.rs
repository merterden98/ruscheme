use eval::Evaluates;
use parser::parser;
use rustyline::error::ReadlineError;

mod ast;
mod eval;
mod parser;
mod repl;

fn main() -> rustyline::Result<()> {
    let mut evaluator = eval::Evaluator::new();
    let mut repl = repl::REPL::new();

    let mut count = 1;
    loop {
        let p = format!("{}> ", count);
        repl.rl.helper_mut().expect("No helper").colored_prompt =
            format!("\x1b[1;32m{}\x1b[0m", &p);
        let readline = repl.rl.readline(&p);
        match readline {
            Ok(line) => match parser(line.as_str()) {
                Ok((_, defns)) => {
                    for defn in defns {
                        match evaluator.eval_defn(defn) {
                            Ok(s) => println!("{}", s),
                            Err(e) => println!("{}", e),
                        };
                    }

                    repl.rl.add_history_entry(line.as_str());
                }
                Err(_) => todo!(),
            },
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Encountered Eof");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        count += 1;
    }
    repl.rl.append_history("history.txt")
}
