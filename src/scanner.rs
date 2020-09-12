use crate::token;
use crate::token::TokenType::*;

type Result_Line = Result<(), (usize, String)>;

pub struct Scanner {
    source: String,
    source_chars: Vec<char>,
    tokens: Vec<token::Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        //let source = String::from(source.replace(" ", ""));
        let source_chars = source.chars().collect::<Vec<char>>();
        Scanner {
            source,
            source_chars,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result_Line {
        //let mut source_copy = self.source.clone();
        //let source_it = source_copy.chars().enumerate();
        //let source_it = self.source.chars().collect::<Vec<_>>().into_iter().enumerate();
        while self.current < self.source_chars.len() {
            self.start = self.current;
            self.consume_token(self.source_chars[self.current])?;
            self.current += 1;
        }
        Ok(())
    }

    fn add_token(&mut self, token_type: token::TokenType, literal: Option<token::LoxType>) -> () {
        //slicing char array returns a copy?
        let lexeme = self.source_chars[self.start..self.current + 1]
            .into_iter()
            .collect::<String>();

        let literal = match literal {
            None => token::LoxType::LoxNil,
            Some(typ) => typ,
        };
        self.tokens.push(token::Token {
            token_type,
            literal,
            lexeme,
            line: self.line,
        });
    }

    fn consume_token(&mut self, ch: char) -> Result_Line {
        match ch {
            '\n' => Ok({
                self.line += 1;
            }),
            '(' => Ok(self.add_token(LEFT_PAREN, None)),
            ')' => Ok(self.add_token(RIGHT_PAREN, None)),
            '{' => Ok(self.add_token(LEFT_BRACE, None)),
            '}' => Ok(self.add_token(RIGHT_BRACE, None)),
            ',' => Ok(self.add_token(COMMA, None)),
            '.' => Ok(self.add_token(DOT, None)),
            '-' => Ok(self.add_token(MINUS, None)),
            '+' => Ok(self.add_token(PLUS, None)),
            ';' => Ok(self.add_token(SEMICOLON, None)),
            '*' => Ok(self.add_token(STAR, None)),
            '!' => {
                let tok_type = if self.check('=') { BANG_EQUAL } else { BANG };
                Ok(self.add_token(tok_type, None))
            }
            '=' => {
                let tok_type = if self.check('=') { EQUAL_EQUAL } else { EQUAL };
                Ok(self.add_token(tok_type, None))
            }
            '<' => {
                let tok_type = if self.check('=') { LESS_EQUAL } else { LESS };
                Ok(self.add_token(tok_type, None))
            }
            '>' => {
                let tok_type = if self.check('=') {
                    GREATER_EQUAL
                } else {
                    GREATER
                };
                Ok(self.add_token(tok_type, None))
            },
            '/' => {
                if (self.check('/')) {
                    loop {
                        match self.peek() {
                            Some('\n') => break,
                            Some(_) => {self.advance();},
                            None => break
                        }
                    }
                    Ok(())
                }
                else {
                    Ok(self.add_token(SLASH, None))
                }
            }
            ch => Err((self.line, format!("Unexpected character ({}).", ch))),
        }
    }

    fn advance(&mut self) -> Option<&char> {
        self.current += 1;
        self.source_chars.get(self.current - 1)
    }

    fn peek(&self) -> Option<&char> {
        self.source_chars.get(self.current + 1)
    }

    fn check(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(&next) if next == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    //this isn't really a test, using this to quickly print values
    #[test]
    fn test() {
        let source = String::from("(){},+==,");
        let mut scanner = Scanner::new(source);
        let res = scanner.scan_tokens();
        for tok in scanner.tokens {
            println!("{}", tok);
        }
        match res {
            Err((line, msg)) => eprintln!("Line {}: {}", line, msg),
            _ => (),
        }
    }
}
