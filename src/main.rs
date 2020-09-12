#![allow(warnings)]
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;
use jlox::token;

macro_rules! error {
    ($error:expr) => {
        eprintln!("Error: {}", $error)
    };
}

struct Lox {
    had_error: bool,
}

impl Lox {
    
    fn new() -> Lox {
        Lox {
            had_error: false
        }
    }
    fn report(&mut self, line: usize, where_: &str, msg: &str) {
        let err = format!("[line {}] Error{}: {}", line, where_, msg);
        error!(err);
        self.had_error = true;
    }
    fn run(&mut self, source: &str) {
        println!("{}", source);
        io::stdout().flush().unwrap();
    }
}

fn main() -> Result<(), io::Error> {
    let args = env::args().collect::<Vec<String>>();

    println!("Args: {:?}", args);

    let mut lox = Lox::new();

    match args.len() {
        1 => {
            println!("Running REPL..");
            //run_prompt(&mut lox);
        }
        2 => run_file(&mut lox, &args[1]).unwrap_or_else(|err| {
            error!(err);
        }),
        _ => {
            println!("Usage: jlox [script]");
            process::exit(64);
        }
    }
    Ok(())
}

fn run_prompt(lox: &mut Lox) {
    let mut buffer: String;
    loop {
        buffer = String::from("");
        print!("> ");
        io::stdout().flush();
        io::stdin().read_line(&mut buffer);
        lox.run(&buffer);
        lox.had_error = false;
    }
}

fn run_file(lox: &mut Lox, filename: &str) -> Result<(), io::Error> {
    let source = fs::read_to_string(filename)?;
    for line in source.lines(){
        lox.run(line);
        if lox.had_error {
            process::exit(64)
        }
    }
    Ok(())
}
