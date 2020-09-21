use crate::token::Token;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    ClosingParenError(Token),
    MissingExprError(Token),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::ClosingParenError(token) => {
                write!(f, "Missing \'(\' on line {}", token.line)
            }
            SyntaxError::MissingExprError(token) => {
                write!(f, "Expected expression on line {}", token.line)
            }
        }
    }
}

impl Error for SyntaxError {}

#[derive(Debug)]
pub enum LexicalError{
    UnexpectedCharacter(char, usize),
    MalformedNumber(usize),
    UnterminatedString(usize)
}
impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::UnexpectedCharacter(ch, line) => {
                write!(f, "Unexpected \'{}\' on line {}", ch, line)
            }
            LexicalError::MalformedNumber(line) => {
                write!(f, "Malformed number on line {}", line)
            },
            LexicalError::UnterminatedString(line) => {
                write!(f, "Unterminated string on line {}", line)
            }
        }
    }
}
impl Error for LexicalError {}
