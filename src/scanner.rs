use crate::error::LexicalError;
use crate::token;
use crate::token::TokenType::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

type ResultLine = Result<(), LexicalError>;

pub struct Scanner {
    source_chars: Vec<char>,
    pub tokens: Vec<token::Token>,
    start: usize,
    current: usize,
    line: usize,
}

//honestly, might as well replace this with a match
lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, token::TokenType> = {
        let mut hm = HashMap::new();
        hm.insert("and", AND);
        hm.insert("class", CLASS);
        hm.insert("else", ELSE);
        hm.insert("false", FALSE);
        hm.insert("for", FOR);
        hm.insert("fun", FUN);
        hm.insert("if", IF);
        hm.insert("nil", NIL);
        hm.insert("or", OR);
        hm.insert("print", PRINT);
        hm.insert("return", RETURN);
        hm.insert("super", SUPER);
        hm.insert("this", THIS);
        hm.insert("true", TRUE);
        hm.insert("var", VAR);
        hm.insert("while", WHILE);
        hm
    };
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        //let source = String::from(source.replace(" ", ""));
        let source_chars = source.chars().collect::<Vec<char>>();
        Scanner {
            source_chars,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn print_tokens(&mut self) {
        for tok in &self.tokens {
            println!("[{:?}]", tok);
        }
    }

    pub fn scan_tokens(&mut self) -> ResultLine {
        //let mut source_copy = self.source.clone();
        //let source_it = source_copy.chars().enumerate();
        //let source_it = self.source.chars().collect::<Vec<_>>().into_iter().enumerate();
        while self.current < self.source_chars.len() {
            self.start = self.current;
            self.consume_token(self.source_chars[self.current])?;
            self.current += 1;
        }
        self.tokens.push(token::Token {
            token_type: EOF,
            literal: None,
            lexeme: String::from(""),
            line: self.line,
        });
        Ok(())
    }

    fn add_token(&mut self, token_type: token::TokenType, literal: Option<token::Literal>) -> () {
        //slicing char array returns a copy?
        let lexeme = self.source_chars[self.start..self.current + 1]
            .into_iter()
            .collect::<String>();

        self.tokens.push(token::Token {
            token_type,
            literal,
            lexeme,
            line: self.line,
        });
    }

    fn consume_token(&mut self, ch: char) -> ResultLine {
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
            '?' => Ok(self.add_token(TERNARY, None)),
            ':' => Ok(self.add_token(COLON, None)),
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
            }
            '/' => {
                if self.check('/') {
                    loop {
                        match self.peek() {
                            Some('\n') => break,
                            Some(_) => {
                                self.advance();
                            }
                            None => break,
                        }
                    }
                } else {
                    self.add_token(SLASH, None)
                }
                Ok(())
            }
            '"' => {
                self.start += 1;
                self.string()
            }
            ch if Scanner::is_digit(ch) => self.number(ch),
            ch if Scanner::is_alpha(ch) => self.identifier(ch),
            ' ' | '\r' | '\t' => Ok(()),
            ch => Err(LexicalError::UnexpectedCharacter(self.line, ch)),
        }
    }

    fn is_digit(ch: char) -> bool {
        ch >= '0' && ch <= '9'
    }

    fn is_alpha(ch: char) -> bool {
        (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch == '_')
    }

    fn is_alpha_numeric(ch: char) -> bool {
        Scanner::is_alpha(ch) || Scanner::is_digit(ch)
    }

    fn identifier(&mut self, alpha: char) -> ResultLine {
        let mut s: String = String::new();
        s.push(alpha);
        while let Some(&ch) = self.peek() {
            if Scanner::is_alpha_numeric(ch) {
                s.push(ch)
            } else {
                break;
            }
            self.advance();
        }
        if let Some(type_) = KEYWORDS.get(&s[..]) {
            match *type_ {
                TRUE => self.add_token(type_.clone(), Some(token::Literal::LoxBool(true))),
                FALSE => self.add_token(type_.clone(), Some(token::Literal::LoxBool(false))),
                NIL => self.add_token(type_.clone(), Some(token::Literal::LoxNil)),
                _ => self.add_token(type_.clone(), None),
            }
        } else {
            self.add_token(IDENTIFIER, None);
        }
        Ok(())
    }

    fn number(&mut self, num: char) -> ResultLine {
        let mut seen_dot: bool = false;
        let mut trailing_dot: bool = false;
        let mut s: String = String::new();
        s.push(num);
        loop {
            match self.peek() {
                None => {
                    if trailing_dot {
                        return Err(LexicalError::MalformedNumber(self.line));
                    } else {
                        break;
                    }
                }
                Some(&ch) if Scanner::is_digit(ch) => {
                    if trailing_dot {
                        trailing_dot = false;
                    }
                    s.push(ch);
                }
                Some('.') => {
                    if seen_dot {
                        return Err(LexicalError::MalformedNumber(self.line));
                    } else {
                        seen_dot = true;
                        trailing_dot = true;
                        s.push('.');
                    }
                }
                _ => break,
            };
            self.advance();
        }
        if trailing_dot {
            return Err(LexicalError::MalformedNumber(self.line));
        }
        self.add_token(NUMBER, Some(token::Literal::LoxNumber(s.parse().unwrap())));
        Ok(())
    }

    fn string(&mut self) -> ResultLine {
        let mut s: String = String::new();
        loop {
            match self.peek() {
                None => break Err(LexicalError::UnterminatedString(self.line)),
                Some('"') => {
                    self.add_token(STRING, Some(token::Literal::LoxString(s)));
                    self.advance();
                    break Ok(());
                }
                Some(&ch) => s.push(ch),
            };
            self.advance();
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
