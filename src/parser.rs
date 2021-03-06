use crate::error::SyntaxError;
use crate::token;
use crate::token::TokenType::*;
use std::collections::VecDeque;

macro_rules! expand_binary {
    ($self:ident, $expr:ident, $match_slice:expr, $right_parse:ident) => {
        while let Some(op) = $self.match_next(&$match_slice) {
            let right = Box::new($self.$right_parse()?);
            $expr = Expr::Binary(Binary {
                left: Box::new($expr),
                operator: op,
                right,
            });
        }
        return Ok($expr);
    };
}

type ResultExpr = Result<Expr, SyntaxError>;
type ResultStmt = Result<Stmt, SyntaxError>;

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
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: token::Token,
    pub args: Vec<Box<Expr>>,
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
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub ident: token::Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    pub name: token::Token,
    pub params: Vec<token::Token>,
    pub body: Vec<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub if_true: Box<Stmt>,
    pub if_false: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub stmt: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: token::Token,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    VarDecl(VarDecl),
    FunDecl(FunDecl),
    Block(Vec<Box<Stmt>>),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    Return(ReturnStmt),
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
        while let Some(_) = self.peek() {
            if self.peek().unwrap().token_type == EOF {
                break;
            }
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

    fn declaration(&mut self) -> ResultStmt {
        if let Some(_) = self.match_next(&[VAR]) {
            return self.var_declaration();
        }
        if let Some(_) = self.match_next(&[FUN]) {
            return self.fun_declaration("function");
        } else {
            return self.statement();
        }
    }

    fn fun_declaration(&mut self, _: &str) -> ResultStmt {
        let name = self.consume(IDENTIFIER)?;
        self.consume(LEFT_PAREN)?;
        let mut params = Vec::new();
        if let None = self.match_next(&[RIGHT_PAREN]) {
            loop {
                if let Some(tok) = self.match_next(&[IDENTIFIER]) {
                    params.push(tok);
                    if let None = self.match_next(&[COMMA]) {
                        break;
                    }
                }
            }
            self.consume(RIGHT_PAREN)?;
        };
        self.consume(LEFT_BRACE)?;
        let block = if let Stmt::Block(block) = self.block()? {
            block
        } else {
            unreachable!()
        };
        Ok(Stmt::FunDecl(FunDecl {
            name,
            params,
            body: block,
        }))
    }

    fn var_declaration(&mut self) -> ResultStmt {
        let ident = self.consume(IDENTIFIER)?;
        let initializer = if let Some(_) = self.match_next(&[EQUAL]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(SEMICOLON)?;
        Ok(Stmt::VarDecl(VarDecl { ident, initializer }))
    }

    fn statement(&mut self) -> ResultStmt {
        if let Some(_) = self.match_next(&[PRINT]) {
            return Ok(self.print_statement()?);
        }
        if let Some(_) = self.match_next(&[LEFT_BRACE]) {
            return Ok(self.block()?);
        }
        if let Some(_) = self.match_next(&[IF]) {
            return Ok(self.if_stmt()?);
        }
        if let Some(_) = self.match_next(&[WHILE]) {
            return Ok(self.while_stmt()?);
        }
        if let Some(_) = self.match_next(&[FOR]) {
            return Ok(self.for_stmt()?);
        }
        if let Some(tok) = self.match_next(&[RETURN]) {
            return Ok(self.return_stmt(tok)?);
        }
        Ok(self.expr_statement()?)
    }

    fn return_stmt(&mut self, keyword: token::Token) -> ResultStmt {
        let expr = self.expression()?;
        self.consume(SEMICOLON)?;
        Ok(Stmt::Return(ReturnStmt {
            keyword,
            expression: Box::new(expr),
        }))
    }

    fn expr_statement(&mut self) -> ResultStmt {
        let expr = self.expression()?;
        self.consume(SEMICOLON)?;
        Ok(Stmt::Expr(expr))
    }

    fn print_statement(&mut self) -> ResultStmt {
        let expr = self.expression()?;
        self.consume(SEMICOLON)?;
        Ok(Stmt::Print(expr))
    }

    fn block(&mut self) -> ResultStmt {
        let mut block_stmts = Vec::new();
        loop {
            if let Some(tok) = self.match_next(&[EOF]) {
                break Err(SyntaxError::ClosingBracket(tok.line));
            }
            if let Some(_) = self.match_next(&[RIGHT_BRACE]) {
                break Ok(Stmt::Block(block_stmts));
            }
            let stmt = self.declaration()?;
            block_stmts.push(Box::new(stmt));
        }
    }

    fn if_stmt(&mut self) -> ResultStmt {
        self.consume(LEFT_PAREN)?;
        let condition = self.expression()?;
        self.consume(RIGHT_PAREN)?;
        let if_true = self.statement()?;
        let if_false = if let Some(_) = self.match_next(&[ELSE]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::IfStmt(IfStmt {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false,
        }))
    }

    fn while_stmt(&mut self) -> ResultStmt {
        self.consume(LEFT_PAREN)?;
        let condition = self.expression()?;
        self.consume(RIGHT_PAREN)?;
        let stmt = self.statement()?;
        Ok(Stmt::WhileStmt(WhileStmt {
            condition: Box::new(condition),
            stmt: Box::new(stmt),
        }))
    }

    fn for_stmt(&mut self) -> ResultStmt {
        self.consume(LEFT_PAREN)?;
        let initializer = if let Some(_) = self.match_next(&[VAR]) {
            Some(self.var_declaration()?)
        } else if let Some(_) = self.match_next(&[SEMICOLON]) {
            None
        } else {
            Some(self.expr_statement()?)
        };

        let condition = if let Some(_) = self.match_next(&[SEMICOLON]) {
            Expr::Literal(token::Literal::LoxBool(true))
        } else {
            let expr = self.expression()?;
            self.consume(SEMICOLON)?;
            expr
        };

        let next = self.peek().unwrap();
        let increment = if next.token_type == RIGHT_PAREN {
            None
        } else if next.token_type == EOF {
            Err(SyntaxError::ClosingParen(next.line))?
        } else {
            let expr = Some(self.expression()?);
            expr
        };

        self.consume(RIGHT_PAREN)?;
        let body = self.statement()?;

        let body = if let Some(increment_expr) = increment {
            let mut vec = Vec::new();
            vec.push(Box::new(body));
            vec.push(Box::new(Stmt::Expr(increment_expr)));
            Stmt::Block(vec)
        } else {
            body
        };

        let desugered_loop = Stmt::WhileStmt(WhileStmt {
            condition: Box::new(condition),
            stmt: Box::new(body),
        });

        let desugered_loop = if let Some(initializer) = initializer {
            let mut vec = Vec::new();
            vec.push(Box::new(initializer));
            vec.push(Box::new(desugered_loop));
            Stmt::Block(vec)
        } else {
            desugered_loop
        };

        Ok(desugered_loop)
    }

    fn expression(&mut self) -> ResultExpr {
        Ok(self.assignment()?)
    }

    fn assignment(&mut self) -> ResultExpr {
        let res = match (self.ternary()?, self.match_next(&[EQUAL])) {
            (expr, None) => expr,
            (Expr::Variable(ident), Some(_)) => Expr::Assignment(Assignment {
                ident,
                expression: Box::new(self.ternary()?),
            }),
            (_, Some(eq_token)) => Err(SyntaxError::InvalidAssignment(eq_token.line))?,
        };
        Ok(res)
    }

    fn ternary(&mut self) -> ResultExpr {
        let mut expr = self.logic_or()?;
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

    fn logic_or(&mut self) -> ResultExpr {
        let mut expr = self.logic_and()?;
        expand_binary!(self, expr, [OR], logic_and);
    }

    fn logic_and(&mut self) -> ResultExpr {
        let mut expr = self.equality()?;
        expand_binary!(self, expr, [AND], equality);
    }
    fn equality(&mut self) -> ResultExpr {
        let mut expr = self.comparison()?;
        expand_binary!(self, expr, [PLUS, MINUS], comparison);
    }

    fn comparison(&mut self) -> ResultExpr {
        let mut expr = self.addition()?;
        expand_binary!(
            self,
            expr,
            [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL],
            addition
        );
    }

    fn addition(&mut self) -> ResultExpr {
        let mut expr = self.multiplication()?;
        expand_binary!(self, expr, [PLUS, MINUS], multiplication);
    }

    fn multiplication(&mut self) -> ResultExpr {
        let mut expr = self.unary()?;
        expand_binary!(self, expr, [STAR, SLASH], unary);
    }

    fn unary(&mut self) -> ResultExpr {
        if let Some(op) = self.match_next(&[BANG, MINUS]) {
            let expr = Box::new(self.unary()?);
            return Ok(Expr::Unary(Unary {
                operator: op,
                right: expr,
            }));
        }
        self.call()
    }

    fn call(&mut self) -> ResultExpr {
        let mut expr = self.primary()?;

        loop {
            if let Some(_) = self.match_next(&[LEFT_PAREN]) {
                expr = self.finish_call(expr)?;
            } else {
                return Ok(expr);
            }
        }
    }

    fn finish_call(&mut self, callee: Expr) -> ResultExpr {
        let mut args = Vec::new();
        let paren_maybe = self.peek().unwrap();
        let paren_maybe = if !(paren_maybe.token_type == RIGHT_PAREN) {
            loop {
                let curr_expr = self.expression()?;
                args.push(Box::new(curr_expr));
                if let None = self.match_next(&[COMMA]) {
                    break self.consume(RIGHT_PAREN)?;
                }
            }
        } else {
            self.consume(RIGHT_PAREN)?
        };
        Ok(Expr::Call(Call {
            callee: Box::new(callee),
            paren: paren_maybe,
            args,
        }))
    }

    fn primary(&mut self) -> ResultExpr {
        if let Some(_) = self.match_next(&[LEFT_PAREN]) {
            let expr = Expr::Grouping(Grouping {
                expression: Box::new(self.expression()?),
            });
            self.consume(RIGHT_PAREN)?;
            return Ok(expr);
        }

        match self.peek() {
            None => panic!("why am i here"),
            Some(tok) => match tok.token_type {
                NUMBER | STRING | TRUE | FALSE | NIL => {
                    Ok(Expr::Literal(self.advance().unwrap().literal.unwrap()))
                }
                IDENTIFIER => Ok(Expr::Variable(self.advance().unwrap())),
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
            Some(tok) if expected == LEFT_PAREN => Err(SyntaxError::OpeningParen(tok.line)),
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
