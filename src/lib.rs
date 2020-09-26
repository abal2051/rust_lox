#![feature(test)]
#![feature(map_entry_replace)]
#![feature(hash_raw_entry)]
#![allow(warnings)]
pub mod error;
pub mod interpreter;
pub mod parser;
pub mod scanner;
mod token;

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;

    extern crate test;
    use test::Bencher;

    #[bench]
    fn it_works(b: &mut Bencher) {
        b.iter(|| {
            let source = String::from("print (5 + 5) > (5 * 5) ? \"addition is greater\" : \"multiplication is greater\";");
            let mut scanner = scanner::Scanner::new(source);
            scanner.scan_tokens();
            let mut parser = parser::Parser::new(VecDeque::from(scanner.tokens));
            let stmts = parser.parse();
            let mut interpreter = interpreter::Interpreter::new();
            interpreter.interpret(stmts);
        });
    }
}
