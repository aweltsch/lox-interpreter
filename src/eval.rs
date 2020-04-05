use crate::expr::*;
use crate::scanning::TokenType;

// FIXME this is replicated the 3rd time!
#[derive(Debug, PartialEq)]
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

pub fn evaluate(expr: &Expr) -> Result<LoxValue, String> {
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
        TokenType::GREATER => greater_than(left_value, right_value),
        TokenType::GREATER_EQUAL => greater_or_equal_to(left_value, right_value),
        TokenType::LESS => less_than(left_value, right_value),
        TokenType::LESS_EQUAL => less_or_equal_to(left_value, right_value),
        TokenType::EQUAL_EQUAL => equal_to(left_value, right_value),
        TokenType::BANG_EQUAL => unequal_to(left_value, right_value),
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
        TokenType::BANG => logical_negate_value(right_value),
        _ => {
            panic!("unreachable code");
        }
    }
}

fn add_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(l + r)),
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
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(l * r)),
        _ => Err(format!("Can not multiply operands {:?} and {:?}", left, right))
    }
}

fn divide_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(l / r)),
        _ => Err(format!("Can not divide operands {:?} and {:?}", left, right))
    }
}

fn subtract_values(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::NUMBER(l - r)),
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

fn greater_than(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::BOOLEAN(l > r)),
        _ => Err(format!("Can not compare operands {:?} and {:?}", left, right))
    }
}

fn greater_or_equal_to(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::BOOLEAN(l >= r)),
        _ => Err(format!("Can not compare operands {:?} and {:?}", left, right))
    }
}

fn less_than(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::BOOLEAN(l < r)),
        _ => Err(format!("Can not compare operands {:?} and {:?}", left, right))
    }
}

fn less_or_equal_to(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    match (&left, &right) {
        (LoxValue::NUMBER(l), LoxValue::NUMBER(r)) => Ok(LoxValue::BOOLEAN(l <= r)),
        _ => Err(format!("Can not compare operands {:?} and {:?}", left, right))
    }
}

fn equal_to(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    Ok(LoxValue::BOOLEAN(left == right))
}

fn unequal_to(left: LoxValue, right: LoxValue) -> Result<LoxValue, String> {
    Ok(LoxValue::BOOLEAN(left != right))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanning::scan_tokens;
    use crate::parser::parse;

    #[test]
    fn test_arithmetic() {
        assert_eq!(negate_value(LoxValue::NUMBER(1.0)).unwrap(), LoxValue::NUMBER(-1.0));
        assert_eq!(logical_negate_value(LoxValue::NIL).unwrap(), LoxValue::BOOLEAN(true));
        assert_eq!(add_values(LoxValue::NUMBER(1.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::NUMBER(3.0));
        assert_eq!(subtract_values(LoxValue::NUMBER(1.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::NUMBER(-1.0));
        assert_eq!(divide_values(LoxValue::NUMBER(1.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::NUMBER(0.5));
        assert_eq!(multiply_values(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::NUMBER(6.0));
        assert_eq!(add_values(LoxValue::STRING("left".to_string()), LoxValue::STRING("right".to_string())).unwrap(),
            LoxValue::STRING("leftright".to_string()));
        assert_eq!(less_than(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(false));
        assert_eq!(greater_than(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(true));
        assert_eq!(greater_or_equal_to(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(true));
        assert_eq!(less_or_equal_to(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(false));
        assert_eq!(equal_to(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(false));
        assert_eq!(unequal_to(LoxValue::NUMBER(3.0), LoxValue::NUMBER(2.0)).unwrap(), LoxValue::BOOLEAN(true));
    }

    #[test]
    fn test_eval() {
        let test_data = &[("13 + 87", LoxValue::NUMBER(100.0)), 
                         ("-(3 + 2) / 2", LoxValue::NUMBER(-2.5)),
                         ("!(3 >= 2) == true", LoxValue::BOOLEAN(false))
        ];
                         
        for (original, expected) in test_data {
            let original_tokens = scan_tokens(&original);
            let stmt = &parse(original_tokens).unwrap()[0];
            let actual = evaluate(stmt.get_expr());

            assert_eq!(actual.unwrap(), *expected, "Value does not match for: {}", original);
        }
    }
}
