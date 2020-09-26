use crate::error::SyntaxError;
use crate::token;
use crate::token::TokenType::*;
use std::collections::VecDeque;

type Result_Expr = Result<Expr, SyntaxError>;
type Result_Stmt = Result<Stmt, SyntaxError>;

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
#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: token::Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Ternary {
    pub condition: Box<Expr>,
    pub if_true: Box<Expr>,
    pub if_false: Box<Expr>,
    pub operator: token::Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub ident: token::Token,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ternary(Ternary),
    Binary(Binary),
    Unary(Unary),
    Literal(token::Literal),
    Grouping(Grouping),
    Variable(token::Token),
    Assignment(Assignment),
}

#[derive(Debug, Clone)]
pub struct Var_Decl {
    pub ident: token::Token,
    pub initializer: Option<Expr>
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var_Decl(Var_Decl)
}

pub struct Parser {
    tokens: VecDeque<token::Token>,
    current: usize,
    pub errors: Vec<SyntaxError>,
}

impl Parser {
    pub fn new(tokens: VecDeque<token::Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        self.program()
    }

    fn program(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.peek().unwrap().token_type != EOF {
            let stmt = self.declaration();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(error) => {
                    self.errors.push(error);
                    self.synchronize();
                }
            }
        }
        stmts
    }

    fn declaration(&mut self) -> Result_Stmt {
        if let Some(_) = self.match_next(&[VAR]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result_Stmt {
        let ident = self.consume(IDENTIFIER)?;
        let initializer = if let Some(_) = self.match_next(&[EQUAL]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON)?;
        Ok(Stmt::Var_Decl(Var_Decl { ident, initializer }))
    }

    fn statement(&mut self) -> Result_Stmt {
        if let Some(_) = self.match_next(&[PRINT]) {
            Ok(self.print_statement()?)
        } else {
            Ok(self.expr_statement()?)
        }
    }

    fn expr_statement(&mut self) -> Result_Stmt {
        let expr = self.expression()?;
        self.consume(SEMICOLON)?;
        Ok(Stmt::Expr(expr))
    }

    fn print_statement(&mut self) -> Result_Stmt {
        let expr = self.expression()?;
        self.consume(SEMICOLON)?;
        Ok(Stmt::Print(expr))
    }

    fn expression(&mut self) -> Result_Expr {
        Ok(self.assignment()?)
    }

    fn assignment(&mut self) -> Result_Expr {
        let res = match (self.ternary()?, self.match_next(&[EQUAL])) {
            (expr, None) => expr,
            (Expr::Variable(ident), Some(eq_token)) => Expr::Assignment(Assignment {
                ident,
                expression: Box::new(self.ternary()?),
            }),
            (_, Some(eq_token)) => Err(SyntaxError::InvalidAssignment(eq_token.line))?,
            _ => unreachable!(),
        };
        Ok(res)
    }

    fn ternary(&mut self) -> Result_Expr {
        let mut expr = self.equality()?;
        if let Some(op) = self.match_next(&[TERNARY]) {
            let if_true = self.equality()?;
            self.consume(COLON)?;
            let if_false = self.equality()?;
            expr = Expr::Ternary(Ternary {
                condition: Box::new(expr),
                if_false: Box::new(if_false),
                if_true: Box::new(if_true),
                operator: op,
            })
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result_Expr {
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

    fn comparison(&mut self) -> Result_Expr {
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

    fn addition(&mut self) -> Result_Expr {
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

    fn multiplication(&mut self) -> Result_Expr {
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

    fn unary(&mut self) -> Result_Expr {
        if let Some(op) = (self.match_next(&[BANG, MINUS])) {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary(Unary {
                operator: op,
                right: expr,
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result_Expr {
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
                IDENTIFIER => Ok(Expr::Variable(tok)),
                _ => Err(SyntaxError::MissingExpr(tok.line)),
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

    fn consume(&mut self, expected: token::TokenType) -> Result<token::Token, SyntaxError> {
        match self.peek() {
            Some(tok) if (tok).token_type == expected => Ok(self.advance().unwrap()),
            Some(tok) if expected == IDENTIFIER => Err(SyntaxError::MissingIdentifier(tok.line)),
            Some(tok) if expected == RIGHT_PAREN => Err(SyntaxError::ClosingParen(tok.line)),
            Some(tok) if expected == COLON => Err(SyntaxError::MissingTernaryColon(tok.line)),
            Some(tok) if expected == SEMICOLON => Err(SyntaxError::MissingSemicolon(tok.line)),
            _ => panic!("Should not have reached this state"),
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
                EOF => {
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
            Expr::Ternary(Ternary {
                condition,
                if_true,
                if_false,
                operator,
            }) => paranthesize(&operator.lexeme, vec![condition, if_true, if_false]),
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
        Expr::Ternary(Ternary {
            condition,
            if_true,
            if_false,
            operator,
        }) => paranthesize(&operator.lexeme, vec![condition, if_true, if_false]),
        _ => panic!(),
    }
}
