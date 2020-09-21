use crate::parser::{Binary, Expr, Grouping, Literal, Unary};
use crate::token::{self, Literal::*, TokenType::*};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter{
        Interpreter {}
    }

    pub fn eval(&self, expr: Expr) -> token::Literal {
        match expr {
            Expr::Literal(literal) => literal,
            Expr::Grouping(grouping) => self.eval_expr(grouping),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            _ => panic!(),
        }
    }

    fn eval_expr(&self, group_expr: Grouping) -> token::Literal {
        self.eval(*group_expr.expression)
    }

    fn eval_unary(&self, unary_expr: Unary) -> token::Literal {
        let Unary { operator, right } = unary_expr;
        let right_val = self.eval(*right);

        match (operator.token_type, right_val) {
            (MINUS, LoxNumber(actual_val)) => LoxNumber(-actual_val),
            (BANG, LoxValue) => LoxBool(!self.isTruthy(LoxValue)),
            _ => panic!(),
        }
    }

    fn eval_binary(&self, bin_expr: Binary) -> token::Literal {
        let Binary {
            left,
            operator,
            right,
        } = bin_expr;
        let left_val = self.eval(*left);
        let right_val = self.eval(*right);
        match (left_val, operator.token_type, right_val) {
            (LoxNumber(left_num), PLUS, LoxNumber(right_num)) => LoxNumber(left_num + right_num),
            (LoxNumber(left_num), STAR, LoxNumber(right_num)) => LoxNumber(left_num * right_num),
            _ => panic!(),
        }
    }

    fn isTruthy(&self, literal: token::Literal) -> bool{
        match literal {
            LoxBool(val) => val,
            LoxNil => false,
            _ => true
        }
    }
}
