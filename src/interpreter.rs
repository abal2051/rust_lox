use crate::error::RuntimeError;
use crate::parser::{Binary, Ternary, Expr, Grouping, Unary};
use crate::token::{self, Literal::*, TokenType::*};

type Result_Interpreter = Result<token::Literal, RuntimeError>;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn eval(&self, expr: Expr) -> Result_Interpreter {
        match expr {
            Expr::Literal(literal) => Ok(literal),
            Expr::Grouping(grouping) => self.eval_expr(grouping),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Ternary(ternary) => self.eval_ternary(ternary),
            _ => panic!(),
        }
    }

    fn eval_expr(&self, group_expr: Grouping) -> Result_Interpreter {
        self.eval(*group_expr.expression)
    }

    fn eval_unary(&self, unary_expr: Unary) -> Result_Interpreter {
        let Unary { operator, right } = unary_expr;
        let right_val = self.eval(*right)?;

        let ret = match (&operator.token_type, right_val) {
            (MINUS, LoxNumber(actual_val)) => LoxNumber(-actual_val),
            (BANG, LoxValue) => LoxBool(!self.isTruthy(LoxValue)),
            (_, LoxType) => Err(RuntimeError::UnaryTypeError(operator, LoxType))?,
        };
        Ok(ret)
    }

    fn eval_binary(&self, bin_expr: Binary) -> Result_Interpreter {
        let Binary {
            left,
            operator,
            right,
        } = bin_expr;
        let left_val = self.eval(*left)?;
        let right_val = self.eval(*right)?;
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
                    operator,
                    LoxNumber(right_num),
                ))?,
            },
            (LoxString(left_string), LoxString(right_string)) => match &operator.token_type {
                PLUS => LoxString(format!("{}{}", left_string, right_string)),
                _ => Err(RuntimeError::BinaryTypeError(
                    LoxString(left_string),
                    operator,
                    LoxString(right_string),
                ))?,
            },
            (AnyTypeLeft, AnyTypeRight) => match &operator.token_type {
                EQUAL_EQUAL => LoxBool(AnyTypeLeft == AnyTypeRight),
                BANG_EQUAL => LoxBool(AnyTypeLeft != AnyTypeRight),
                _ => Err(RuntimeError::BinaryTypeError(
                    AnyTypeLeft,
                    operator,
                    AnyTypeRight,
                ))?,
            },
            _ => panic!(),
        };
        Ok(ret)
    }

    fn eval_ternary(&self, tern_expr: Ternary) -> Result_Interpreter {
        let Ternary {
            condition,
            if_true,
            if_false,
            ..
        } = tern_expr;
        let condition = self.isTruthy(self.eval(*condition)?);
        let if_true = self.eval(*if_true)?;
        if condition {
            Ok(if_true)
        }
        else {
            let if_false = self.eval(*if_false)?;
            Ok(if_false)
        }
    }

    fn isTruthy(&self, literal: token::Literal) -> bool {
        match literal {
            LoxBool(val) => val,
            LoxNil => false,
            _ => true,
        }
    }
}
