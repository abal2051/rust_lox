#![allow(warnings)]
use jlox::error;
use jlox::parser;
use jlox::scanner;
use jlox::interpreter;
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

struct Lox {
    had_error: bool,
}

impl Lox {
    fn new() -> Lox {
        Lox { had_error: false }
    }
    fn run(&mut self, source: &str) {
        let mut scanner = scanner::Scanner::new(String::from(source));
        match scanner.scan_tokens() {
            Err(err) => { eprintln!("LexicalError: {}", err); return},
            _ => (),
        }
        for tok in scanner.tokens.iter() {
            print!("[{:?}],", tok);
        }
        println!();
        let mut parser = parser::Parser::new(VecDeque::from(scanner.tokens));
        let interpreter = interpreter::Interpreter::new();
        let expr = match parser.expression() {
            Err(err) => eprintln!("ParserError: {}", err),
            Ok(expr) => {
                let res = interpreter.eval(expr.clone());
                println!("{}", parser::print_expr(&expr));
                println!("{:?}", res);
            }
        };

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
            2 => run_file(&mut lox, &args[1]).unwrap_or_else(|err| {
                error!(err);
            }),
            _ => {
                println!("Usage: jlox [script]");
                process::exit(64);
            }
        }
//    let mut scanner = Scanner::new(String::from("1 + 1"));
//    scanner.scan_tokens();
//    let mut parser = jlox::parser::Parser::new(std::collections::VecDeque::from(scanner.tokens));
//    let res = parser.expression();
//    println!("{:?}", res);
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
    for line in source.lines() {
        lox.run(line);
        if lox.had_error {
            process::exit(64)
        }
    }
    Ok(())
}
