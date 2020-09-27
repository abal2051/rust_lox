use crate::error::RuntimeError;
use crate::parser::{
    Assignment, Binary, Expr, Grouping, IfStmt, Stmt, Ternary, Unary, VarDecl, WhileStmt,
};
use crate::token::{self, Literal::*, TokenType::*};
use std::collections::{
    hash_map::RandomState, hash_map::RawEntryMut, hash_map::RawOccupiedEntryMut, HashMap,
};

type ResultEvaluation = Result<token::Literal, RuntimeError>;
type ResultExecution = Result<(), RuntimeError>;

pub struct Interpreter {
    env: Option<Environment>,
}

struct Environment {
    env: HashMap<String, token::Literal>,
    parent_env: Option<Box<Environment>>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            env: HashMap::new(),
            parent_env: None,
        }
    }

    fn set_parent(&mut self, parent_env: Option<Environment>) {
        self.parent_env = Some(Box::new(parent_env.unwrap()));
    }

    fn define(&mut self, ident: String, literal: Option<token::Literal>) {
        if let Some(value) = literal {
            self.env.insert(ident, value);
        } else {
            self.env.insert(ident, LoxNil);
        }
    }

    fn get_entry(
        &mut self,
        ident: &String,
    ) -> Result<RawOccupiedEntryMut<String, token::Literal, RandomState>, RuntimeError> {
        if let RawEntryMut::Occupied(entry) = self.env.raw_entry_mut().from_key(ident) {
            return Ok(entry);
        } else {
            if let None = self.parent_env {
                return Err(RuntimeError::UndefinedVariable(ident.clone()));
            } else {
                let parent_env = self.parent_env.as_mut().unwrap();
                return Ok(parent_env.get_entry(ident)?);
            }
        };
    }

    fn get(&mut self, ident: &String) -> ResultEvaluation {
        let entry = self.get_entry(ident)?;
        Ok(entry.get().clone())
    }

    fn assign(&mut self, ident: &String, literal: token::Literal) -> ResultEvaluation {
        let mut entry = self.get_entry(ident)?;
        let value = entry.get_mut();
        *value = literal.clone();
        Ok(literal)
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Some(Environment::new()),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> ResultExecution {
        for stmt in stmts.into_iter() {
            self.execute(&stmt)?
        }
        Ok(())
    }

    pub fn execute(&mut self, stmt: &Stmt) -> ResultExecution {
        match &stmt {
            Stmt::Expr(expr) => {
                self.evaluate(&expr)?;
            }
            Stmt::Print(expr) => {
                self.exec_print(expr)?;
            }
            Stmt::VarDecl(var_decl) => {
                self.exec_decl(var_decl)?;
            }
            Stmt::Block(stmts) => {
                self.exec_block(stmts)?;
            }
            Stmt::IfStmt(if_stmt) => {
                self.exec_if(if_stmt)?;
            }
            Stmt::WhileStmt(while_stmt) => {
                self.exec_while(while_stmt)?;
            }
        }
        Ok(())
    }

    fn exec_print(&mut self, expr: &Expr) -> ResultExecution {
        let res = self.evaluate(&expr)?;
        println!("{}", res);
        Ok(())
    }

    fn exec_decl(&mut self, var_decl: &VarDecl) -> ResultExecution {
        match var_decl {
            VarDecl {
                ident,
                initializer: Some(expr),
            } => {
                let res = self.evaluate(&expr)?;
                self.env
                    .as_mut()
                    .unwrap()
                    .define(ident.lexeme.clone(), Some(res));
            }

            VarDecl {
                ident,
                initializer: None,
            } => {
                self.env
                    .as_mut()
                    .unwrap()
                    .define(ident.lexeme.clone(), None);
            }
        }
        Ok(())
    }

    fn exec_block(&mut self, stmts: &Vec<Box<Stmt>>) -> ResultExecution {
        let mut new_env = Environment::new();
        new_env.set_parent(self.env.take());
        self.env = Some(new_env);
        for stmt in stmts {
            self.execute(&stmt)?;
        }
        let env = self.env.take().unwrap();
        match env.parent_env {
            Some(env) => {
                self.env = Some(*env);
            }
            None => {
                self.env = Some(env);
            }
        }
        Ok(())
    }

    fn exec_if(&mut self, if_stmt: &IfStmt) -> ResultExecution {
        let IfStmt {
            condition,
            if_true,
            if_false,
        } = if_stmt;
        let condition = self.evaluate(condition)?;
        match (Interpreter::is_truthy(condition), if_false) {
            (true, _) => {
                self.execute(if_true)?;
            }
            (false, Some(if_false)) => {
                self.execute(if_false)?;
            }
            _ => (),
        };
        Ok(())
    }

    fn exec_while(&mut self, while_stmt: &WhileStmt) -> ResultExecution {
        let WhileStmt { condition, stmt } = while_stmt;

        while Interpreter::is_truthy(self.evaluate(&condition)?) {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> ResultEvaluation {
        match expr {
            Expr::Literal(literal) => Ok(literal.clone()),
            Expr::Grouping(grouping) => self.eval_grouping(grouping),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Ternary(ternary) => self.eval_ternary(ternary),
            Expr::Variable(token) => self.env.as_mut().unwrap().get(&token.lexeme),
            Expr::Assignment(assignment) => self.eval_assignment(assignment),
        }
    }

    fn eval_assignment(&mut self, assignment_expr: &Assignment) -> ResultEvaluation {
        let Assignment { ident, expression } = assignment_expr;
        let r_value = self.evaluate(expression)?;
        self.env.as_mut().unwrap().assign(&ident.lexeme, r_value)
    }

    fn eval_grouping(&mut self, group_expr: &Grouping) -> ResultEvaluation {
        self.evaluate(&*group_expr.expression)
    }

    fn eval_unary(&mut self, unary_expr: &Unary) -> ResultEvaluation {
        let Unary { operator, right } = unary_expr;
        let right_val = self.evaluate(right)?;

        let ret = match (&operator.token_type, right_val) {
            (MINUS, LoxNumber(actual_val)) => LoxNumber(-actual_val),
            (BANG, lox_value) => LoxBool(!Interpreter::is_truthy(lox_value)),
            (_, lox_type) => Err(RuntimeError::UnaryTypeError(operator.clone(), lox_type))?,
        };
        Ok(ret)
    }

    fn eval_binary(&mut self, bin_expr: &Binary) -> ResultEvaluation {
        let Binary {
            left,
            operator,
            right,
        } = bin_expr;
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?; //not short-circuiting here
        let ret = match (left_val, right_val) {
            (LoxNumber(left_num), LoxNumber(right_num)) => match &operator.token_type {
                PLUS => LoxNumber(left_num + right_num),
                MINUS => LoxNumber(left_num - right_num),
                STAR => LoxNumber(left_num * right_num),
                SLASH => LoxNumber(left_num / right_num),
                GREATER => LoxBool(left_num > right_num),
                GREATER_EQUAL => LoxBool(left_num >= right_num),
                LESS => LoxBool(left_num < right_num),
                LESS_EQUAL => LoxBool(left_num <= right_num),
                EQUAL_EQUAL => LoxBool(left_num == right_num),
                BANG_EQUAL => LoxBool(left_num != right_num),
                _ => Err(RuntimeError::BinaryTypeError(
                    LoxNumber(left_num),
                    operator.clone(),
                    LoxNumber(right_num),
                ))?,
            },
            (LoxString(left_string), LoxString(right_string)) => match &operator.token_type {
                PLUS => LoxString(format!("{}{}", left_string, right_string)),
                _ => Err(RuntimeError::BinaryTypeError(
                    LoxString(left_string),
                    operator.clone(),
                    LoxString(right_string),
                ))?,
            },
            (any_type_left, any_type_right) => match &operator.token_type {
                EQUAL_EQUAL => LoxBool(any_type_left == any_type_right),
                BANG_EQUAL => LoxBool(any_type_left != any_type_right),
                AND => self.eval_and(any_type_left, any_type_right),
                OR => self.eval_or(any_type_left, any_type_right),
                _ => Err(RuntimeError::BinaryTypeError(
                    any_type_left,
                    operator.clone(),
                    any_type_right,
                ))?,
            },
        };
        Ok(ret)
    }

    fn eval_and(&mut self, left: token::Literal, right: token::Literal) -> token::Literal {
        if !Interpreter::is_truthy(left.clone()) {
            left
        } else {
            right
        }
    }

    fn eval_or(&mut self, left: token::Literal, right: token::Literal) -> token::Literal {
        if Interpreter::is_truthy(left.clone()) {
            left
        } else {
            right
        }
    }

    fn eval_ternary(&mut self, tern_expr: &Ternary) -> ResultEvaluation {
        let Ternary {
            condition,
            if_true,
            if_false,
            ..
        } = tern_expr;
        let condition = self.evaluate(&condition)?;
        let condition = Interpreter::is_truthy(condition);
        let if_true = self.evaluate(&if_true)?;
        if condition {
            Ok(if_true)
        } else {
            let if_false = self.evaluate(&if_false)?;
            Ok(if_false)
        }
    }

    fn is_truthy(literal: token::Literal) -> bool {
        match literal {
            LoxBool(val) => val,
            LoxNil => false,
            _ => true,
        }
    }
}
