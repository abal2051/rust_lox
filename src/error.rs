use crate::token;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    ClosingParen(usize),
    MissingExpr(usize),
    MissingTernaryColon(usize),
    MissingSemicolon(usize),
    MissingIdentifier(usize),
    InvalidAssignment(usize),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::ClosingParen(line) => write!(f, "[line {}] Missing \')\'", line),
            SyntaxError::MissingExpr(line) => {
                write!(f, "[line {}] Expected expression", line)
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
            SyntaxError::InvalidAssignment(line) => {
                write!(f, "[line {}] Invalid assignment target", line)
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
    UndefinedVariable(String),
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
            RuntimeError::UndefinedVariable(ident) => write!(
                f,
                "Undefined variable \"{}\"",
                ident
            ),
        }
    }
}

impl Error for RuntimeError {}
