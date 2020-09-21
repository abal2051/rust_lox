use crate::error::SyntaxError;
use crate::token;
use crate::token::TokenType::*;
use std::collections::VecDeque;

type Result_Parser = Result<Expr, SyntaxError>;

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: token::Token,
    pub right: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}
#[derive(Debug,Clone)]
pub struct Unary {
    pub operator: token::Token,
    pub right: Box<Expr>,
}

#[derive(Debug,Clone)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(token::Literal),
    Grouping(Grouping),
}

pub struct Parser {
    tokens: VecDeque<token::Token>,
    current: usize,
    errors: Vec<SyntaxError>,
}

impl Parser {
    pub fn new(tokens: VecDeque<token::Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn expression(&mut self) -> Result_Parser {
        Ok(self.equality()?)
    }

    fn equality(&mut self) -> Result_Parser {
        let mut expr = self.comparison()?;

        while let Some(op) = (self.match_next(&[BANG_EQUAL, EQUAL_EQUAL])) {
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: op,
                right,
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result_Parser {
        let mut expr = self.addition()?;

        while let Some(op) = (self.match_next(&[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL])) {
            let right = Box::new(self.addition()?);
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: op,
                right,
            });
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result_Parser {
        let mut expr = self.multiplication()?;

        while let Some(op) = (self.match_next(&[PLUS, MINUS])) {
            let right = Box::new(self.multiplication()?);
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: op,
                right,
            });
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result_Parser {
        let mut expr = self.unary()?;

        while let Some(op) = (self.match_next(&[STAR, SLASH])) {
            let right = Box::new(self.unary()?);
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: op,
                right,
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result_Parser {
        if let Some(op) = (self.match_next(&[BANG, MINUS])) {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary(Unary {
                operator: op,
                right: expr,
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result_Parser {
        if let Some(_) = (self.match_next(&[LEFT_PAREN])) {
            let expr = Expr::Grouping(Grouping {
                expression: Box::new(self.expression()?),
            });
            self.consume(RIGHT_PAREN)?;
            return Ok(expr);
        }

        match self.advance() {
            None => panic!("why am i here"),
            Some(tok) => match tok.token_type {
                NUMBER | STRING | TRUE | FALSE | NIL => Ok(Expr::Literal(tok.literal.unwrap())),
                _ => Err(SyntaxError::MissingExprError(tok)),
            },
        }
    }

    fn peek(&self) -> Option<&token::Token> {
        self.tokens.front()
    }

    fn advance(&mut self) -> Option<token::Token> {
        self.current += 1;
        self.tokens.pop_front()
    }

    fn check(&self, tok_type: token::TokenType) -> bool {
        match self.peek() {
            Some(tok) => tok.token_type == tok_type,
            None => false,
        }
    }

    fn match_next(&mut self, tok_types: &[token::TokenType]) -> Option<token::Token> {
        for tok_type in tok_types.into_iter() {
            if self.check((tok_type).clone()) {
                return self.advance();
            }
        }
        None
    }

    fn consume(&mut self, expected: token::TokenType) -> Result<(), SyntaxError> {
        match self.peek() {
            None => panic!("Should not have reached this state"),
            Some(tok) if (tok).token_type == expected => Ok(()),
            Some(tok) => Err(SyntaxError::ClosingParenError(tok.clone())),
        }
    }

    fn synchronize(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.token_type {
                SEMICOLON => {
                    self.advance();
                    return;
                }
                CLASS => {
                    return;
                }
                FUN => {
                    return;
                }
                VAR => {
                    return;
                }
                FOR => {
                    return;
                }
                IF => {
                    return;
                }
                WHILE => {
                    return;
                }
                PRINT => {
                    return;
                }
                RETURN => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}

fn paranthesize(name: &str, exprs: Vec<&Expr>) -> String {
    let mut s = format!("({}", name);
    for expr in exprs.into_iter() {
        let res = match expr {
            Expr::Literal(value) => format!("{:?}", value),
            Expr::Grouping(Grouping { expression }) => paranthesize("group", vec![expression]),
            Expr::Unary(Unary { operator, right }) => {
                paranthesize(&operator.lexeme[..], vec![right])
            }
            Expr::Binary(Binary {
                left,
                operator,
                right,
            }) => paranthesize(&operator.lexeme[..], vec![left, right]),
            _ => String::from(""),
        };
        s.push(' ');
        s.push_str(&res[..]);
    }
    s.push(')');
    s
}

pub fn print_expr(expr: &Expr) -> String {
    let e = expr;
    match e {
        Expr::Literal(value) => format!("{:?}", value),
        Expr::Grouping(Grouping { expression }) => paranthesize("group", vec![expression]),
        Expr::Unary(Unary { operator, right }) => paranthesize(&operator.lexeme[..], vec![right]),
        Expr::Binary(Binary {
            left,
            operator,
            right,
        }) => paranthesize(&operator.lexeme[..], vec![left, right]),
    }
}
