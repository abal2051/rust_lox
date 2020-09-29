#![allow(warnings)]
use jlox::error;
use jlox::interpreter;
use jlox::parser;
use jlox::scanner;
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

struct Lox {
    had_error: bool,
    interpreter: interpreter::Interpreter,
}

impl Lox {
    fn new() -> Lox {
        Lox {
            had_error: false,
            interpreter: interpreter::Interpreter::new(),
        }
    }
    fn run(&mut self, source: &str) {
        let mut scanner = scanner::Scanner::new(String::from(source));
        match scanner.scan_tokens() {
            Err(err) => {
                eprintln!("LexicalError: {}", err);
                return;
            }
            _ => (),
        }
        //scanner.print_tokens();
        let mut parser = parser::Parser::new(VecDeque::from(scanner.tokens));
        let parse_tree = parser.parse();
        for error in &parser.errors {
            eprintln!("{}", error)
        }
        if parser.errors.len() > 0 {
            return;
        }
        //println!("{:#?}", parse_tree);
        match self.interpreter.interpret(parse_tree) {
            Err(msg) => eprintln!("RuntimeError: {:?}", msg),
            _ => (),
        }
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
            run_prompt(&mut lox);
        }
        2 => run_file(&mut lox, &args[1]).unwrap_or_else(|err| eprintln!("{}", err)),
        _ => {
            println!("Usage: jlox [script]");
            process::exit(64);
        }
    }
    //        let source = String::from("print (5 + 5) > (5 * 5) ? \"addition is greater\" : \"multiplication is greater\";");
    //        let mut scanner = scanner::Scanner::new(source);
    //        scanner.scan_tokens();
    //        let mut parser = parser::Parser::new(VecDeque::from(scanner.tokens));
    //        let stmts = parser.parse();
    //        let interpreter = interpreter::Interpreter::new();
    //        interpreter.interpret(stmts);
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
    lox.run(&source);
    // for line in source.lines() {
    //     lox.run(line);
    //     if lox.had_error {
    //         process::exit(64)
    //     }
    // }
    Ok(())
}
