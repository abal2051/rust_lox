use crate::token;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    ClosingParenError(usize),
    MissingExprError(usize),
    MissingTernaryColon(usize),
    MissingSemicolon(usize),
    MissingIdentifier(usize)
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::ClosingParenError(line) => write!(f, "[line {}] Missing \')\'", line),
            SyntaxError::MissingExprError(line) => {
                write!(f, "[line {}] Expected expression on line", line)
            },
            SyntaxError::MissingTernaryColon(line) => {
                write!(f, "[line {}] Missing \":\" inside ternary expression", line)
            },
            SyntaxError::MissingSemicolon(line) => {
                write!(f, "[line {}] Missing \";\" after expression", line)
            }
            SyntaxError::MissingIdentifier(line) => {
                write!(f, "[line {}] Expected identifier after \"var\"", line)
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
                write!(f, "[line {}] Unexpected \'{}\'", line, ch)
            }
            LexicalError::MalformedNumber(line) => write!(f, "[line {}] Malformed number", line),
            LexicalError::UnterminatedString(line) => {
                write!(f, "[line {}] Unterminated string", line)
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
