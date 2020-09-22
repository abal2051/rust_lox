use crate::token;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    ClosingParenError(usize),
    MissingExprError(usize),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::ClosingParenError(line) => write!(f, "Missing \'(\' on line {}", line),
            SyntaxError::MissingExprError(line) => {
                write!(f, "Expected expression")
            }
        }
    }
}

impl Error for SyntaxError {}

#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(usize, char),
    MalformedNumber(usize),
    UnterminatedString(usize),
}
impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::UnexpectedCharacter(ch, line) => {
                write!(f, "Unexpected \'{}\'", ch)
            }
            LexicalError::MalformedNumber(line) => write!(f, "Malformed number on line {}", line),
            LexicalError::UnterminatedString(line) => {
                write!(f, "Unterminated string")
            }
        }
    }
}
impl Error for LexicalError {}

#[derive(Debug)]
pub enum RuntimeError {
    BinaryTypeError(token::Literal, token::Token, token::Literal),
    UnaryTypeError(token::Token, token::Literal),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::BinaryTypeError(left, op, right) => write!(
                f,
                "No binary operator \"{}\" for operands of type ({}, {})",
                op.lexeme, left, right
            ),
            RuntimeError::UnaryTypeError(op, right) => write!(
                f,
                "No unary operator \"{}\" for operand of type ({})",
                op.lexeme, right
            ),
        }
    }
}

impl Error for RuntimeError {}
