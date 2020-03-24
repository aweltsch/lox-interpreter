use crate::expr::*;

// FIXME this is replicated the 3rd time!
pub enum LoxValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String)
}

fn evaluate(expr: Expr) -> LoxValue {
    match expr {
        Expr::BINARY(b) => evaluate_binary(b),
        Expr::GROUPING(g) => evaluate_grouping(g),
        Expr::LITERAL(l) => evaluate_literal(l),
        Expr::UNARY(u) => evaluate_unary(u)
    }
}

fn evaluate_literal(l: Literal) -> LoxValue {
    match l {
        Literal::NIL => LoxValue::NIL,
        Literal::BOOLEAN(b) => LoxValue::BOOLEAN(b),
        Literal::NUMBER(n) => LoxValue::NUMBER(n),
        Literal::STRING(s) => LoxValue::STRING(s)
    }
}

fn evaluate_binary(b: Binary) -> LoxValue {
    LoxValue::NIL
}
fn evaluate_grouping(g: Grouping) -> LoxValue {
    evaluate(*g.expression)
}
fn evaluate_unary(u: Unary) -> LoxValue {
    let right_value = evaluate(*u.right);
    right_value
}
