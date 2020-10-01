#![allow(non_camel_case_types)]

use std::fmt;

//surprised enums don't implement clone by default
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    TERNARY,
    COLON,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

//probably should move this out of src/tokens at this point
use crate::interpreter::Function;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    LoxBool(bool),
    LoxString(String),
    LoxNumber(f64),
    LoxNil,
    LoxFunc(Function),
    LoxFuncPtr(*const Function),
}

struct FuncPtr {
    ptr: *const Function,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::LoxBool(x) => write!(f, "{}", x),
            Literal::LoxNumber(x) => write!(f, "{}", x),
            Literal::LoxString(x) => write!(f, "{}", x),
            Literal::LoxNil => write!(f, "Nil"),
            Literal::LoxFunc(fun_decl) => write!(f, "{}", fun_decl.0.name),
            Literal::LoxFuncPtr(fun_ptr) => {
                write!(f, "function {}", unsafe { &(**fun_ptr).0.name.lexeme })
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Option<Literal>,
    pub lexeme: String,
    pub line: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {:?} {:?}",
            self.lexeme, self.token_type, self.literal
        )
    }
}
