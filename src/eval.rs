#![feature(let_chains)]
use crate::expr::*;
use crate::scanning::TokenType;

// FIXME this is replicated the 3rd time!
#[derive(Debug)]
pub enum LoxValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String)
}

impl LoxValue {
    fn to_boolean(&self) -> LoxValue {
        match self {
            LoxValue::NIL => LoxValue::BOOLEAN(false),
            LoxValue::BOOLEAN(b) => LoxValue::BOOLEAN(*b),
            _ => LoxValue::BOOLEAN(true)
        }
    }
}

fn evaluate(expr: &Expr) -> Result<LoxValue, String> {
    match expr {
        Expr::BINARY(b) => evaluate_binary(b),
        Expr::GROUPING(g) => evaluate_grouping(g),
        Expr::LITERAL(l) => evaluate_literal(l),
        Expr::UNARY(u) => evaluate_unary(u)
    }
}

fn evaluate_literal(l: &Literal) -> Result<LoxValue, String> {
    Ok(match l {
        Literal::NIL => LoxValue::NIL,
        Literal::BOOLEAN(b) => LoxValue::BOOLEAN(*b),
        Literal::NUMBER(n) => LoxValue::NUMBER(*n),
        Literal::STRING(s) => LoxValue::STRING(s.clone())
    })
}

fn evaluate_binary(b: &Binary) -> Result<LoxValue, String> {
    let left_value = evaluate(&b.left)?;
    let right_value = evaluate(&b.right)?;
    match &b.operator.token_type {
        TokenType::MINUS => subtract_values(left_value, right_value),
        TokenType::PLUS => add_values(left_value, right_value),
        TokenType::SLASH => divide_values(left_value, right_value),
        TokenType::STAR => multiply_values(left_value, right_value),
        a => {
            panic!("Programming Error: TokenType {:?} should never be part of a binary expression.", a);
        }
    }
}

fn evaluate_grouping(g: &Grouping) -> Result<LoxValue, String> {
    evaluate(&g.expression)
}

fn evaluate_unary(u: &Unary) -> Result<LoxValue, String> {
    let right_value = evaluate(&u.right)?;
    match u.operator.token_type {
        TokenType::MINUS => negate_value(right_value),
        // FIXME this is not correct
        TokenType::BANG => logical_negate_value(right_value),
        _ => {
            panic!("unreachable code");
        }
    }
}

fn add_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(r * l)),
        (LoxValue::STRING(s), LoxValue::STRING(t)) => {
            let mut res = String::new();
            res.push_str(s);
            res.push_str(t);
            Ok(LoxValue::STRING(res))
        },
        _ => Err(format!("Can not add operands {:?} and {:?}", left, right))
    }
}

fn multiply_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(r * l)),
        _ => Err(format!("Can not multiply operands {:?} and {:?}", left, right))
    }
}

fn divide_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(r / l)),
        _ => Err(format!("Can not divide operands {:?} and {:?}", left, right))
    }
}

fn subtract_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(r - l)),
        _ => Err(format!("Can not subtract operands {:?} and {:?}", left, right))
    }
}

fn negate_value(value: LoxValue) -> Result<LoxValue, String> {
    if let LoxValue::NUMBER(n) = value {
        Ok(LoxValue::NUMBER(-n))
    } else {
        Err("can not negate non-number type".to_string())
    }
}

fn logical_negate_value(value: LoxValue) -> Result<LoxValue, String> {
    if let LoxValue::BOOLEAN(b) = value.to_boolean() {
        Ok(LoxValue::BOOLEAN(!b))
    } else {
        panic!("Programming error: should be able to negate every value.");
    }
}
