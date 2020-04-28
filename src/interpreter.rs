use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::ptr;

use crate::expr::*;
use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::parser::Statement;
use crate::parser::ParseError;

// FIXME this is replicated the 3rd time!
#[derive(Debug, PartialEq, Clone)]
pub enum LoxValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String),
    FUNCTION(Rc<LoxFunction>) // it would be cooler if we could have Rc<dyn LoxCallable> but that won't compile
}

trait LoxCallable  {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> Result<LoxValue, RuntimeError>;
    fn arity(&self) -> usize;
}

impl PartialEq for dyn LoxCallable {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

impl fmt::Debug for dyn LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // default implementation!
        f.debug_struct("<native fn>").finish()
    }

}

#[derive(Debug)]
pub enum LoxFunction {
    NATIVE(&'static dyn LoxCallable),
    INTERPRETER(Token, Vec<Token>, Vec<Statement>)
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LoxFunction::NATIVE(c_self) => if let LoxFunction::NATIVE(c_other) = other {
                c_self == c_other
            } else {
                false
            }
            LoxFunction::INTERPRETER(t_self, _, _) => if let LoxFunction::INTERPRETER(t_other, _, _) = other {
                t_self == t_other
            } else {
                false
            }
        }
    }
}



pub type RuntimeError = String;

impl LoxFunction {
    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> Result<LoxValue, RuntimeError> {
        match self {
            LoxFunction::NATIVE(c) => c.call(interpreter, arguments),
            LoxFunction::INTERPRETER(name, params, body) => {
                panic!("not implemented")
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxFunction::NATIVE(c) => c.arity(),
            LoxFunction::INTERPRETER(_, params, _) => params.len()
        }
    }
}

impl LoxValue {
    fn to_boolean(&self) -> LoxValue {
        match self {
            LoxValue::NIL => LoxValue::BOOLEAN(false),
            LoxValue::BOOLEAN(b) => LoxValue::BOOLEAN(*b),
            _ => LoxValue::BOOLEAN(true)
        }
    }

    fn to_native_boolean(&self) -> bool {
        match self.to_boolean() {
            LoxValue::BOOLEAN(b) => b,
            _ => panic!("programming error, to boolean should always result in LoxValue::BOOLEAN")
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = match self {
            LoxValue::NIL => "nil".to_string(),
            LoxValue::BOOLEAN(b) => b.to_string(),
            LoxValue::NUMBER(n) => n.to_string(),
            LoxValue::STRING(s) => format!("{}", s),
            LoxValue::FUNCTION(f) => panic!("not implemented")
        };
        write!(f, "{}", output)
    }
}

pub struct Interpreter {
    environment: Environment
}

pub struct Environment {
    variable_maps: Vec<HashMap<String, LoxValue>>
}

impl Environment {
    pub fn new() -> Self {
        Environment { variable_maps: vec![HashMap::new()] }
    }

    fn is_valid_environment(&self) -> bool {
        self.variable_maps.len() > 0
    }

    pub fn define(&mut self, name: &str, value: LoxValue) {
        assert!(self.is_valid_environment());
        self.variable_maps.last_mut().unwrap().insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<LoxValue, String> {
        assert!(self.is_valid_environment());
        for scope in self.variable_maps.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val.clone());
            }
        }
        Err(format!("Undefined variable {}.", name))
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<LoxValue, String> {
        assert!(self.is_valid_environment());
        for scope in self.variable_maps.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value.clone());
                return Ok(value);
            }
        }
        Err(format!("Undefined variable {}.", name))
    }

    pub fn add_scope(&mut self) {
        assert!(self.is_valid_environment());
        self.variable_maps.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.is_valid_environment());
        self.variable_maps.pop();
        assert!(self.is_valid_environment());
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { environment: Environment::new() }
    }

    pub fn interpret(&mut self, stmts: &Vec<Statement>) {
        for stmt in stmts {
            let result = self.evaluate_statement(stmt);
            if let Err(s) = result {
                panic!("Error {}", s);
            }
        }
    }

    fn evaluate_statement(&mut self, stmt: &Statement) -> Result<LoxValue, ParseError> {
        match stmt {
            Statement::PRINT(expr) => {
                let result = self.evaluate(expr)?;
                println!("{}", result);
                Ok(result)
            },
            Statement::EXPRESSION(expr) => {
                self.evaluate(expr)
            },
            Statement::VAR(name, initializer) => {
                let value = self.evaluate(initializer)?;
                self.environment.define(name, value);
                Ok(LoxValue::NIL)
            },
            Statement::BLOCK(stmts) => {
                self.execute_block(stmts);
                Ok(LoxValue::NIL)
            },
            Statement::IF(if_stmt) => {
                let condition_outcome = self.evaluate(&if_stmt.condition)?;

                if condition_outcome.to_native_boolean() {
                    self.evaluate_statement(&if_stmt.then_branch)
                } else {
                    if let Some(else_branch) = &if_stmt.else_branch {
                        self.evaluate_statement(else_branch)
                    } else {
                        Ok(LoxValue::NIL)
                    }
                }
            },
            Statement::WHILE(condition, body) => {
                while self.evaluate(&condition)?.to_native_boolean() {
                    self.evaluate_statement(&body)?;
                }
                Ok(LoxValue::NIL)
            }
        }
    }
    pub fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, String> {
        match expr {
            Expr::BINARY(b) => self.evaluate_binary(b),
            Expr::GROUPING(g) => self.evaluate_grouping(g),
            Expr::LITERAL(l) => self.evaluate_literal(l),
            Expr::LOGICAL(l) => self.evaluate_logical(l),
            Expr::UNARY(u) => self.evaluate_unary(u),
            Expr::VARIABLE(v) => self.evaluate_variable(v),
            Expr::ASSIGNMENT(a) => self.evaluate_assignment(a),
            Expr::CALL(c) => self.evaluate_function_call(c)
        }
    }

    fn execute_block(&mut self, stmts: &Vec<Box<Statement>>) {
        self.environment.add_scope();
        for stmt in stmts {
            self.evaluate_statement(stmt);
        }
        self.environment.pop_scope();
    }

    fn evaluate_assignment(&mut self, a: &Assignment) -> Result<LoxValue, String> {
        let value = self.evaluate(&a.value)?;
        self.environment.assign(&a.name, value)
    }

    fn evaluate_variable(&mut self, v: &Variable) -> Result<LoxValue, String> {
        self.environment.get(&v.name).map(|x| x)
    }

    fn evaluate_literal(&mut self, l: &Literal) -> Result<LoxValue, String> {
        Ok(match l {
            Literal::NIL => LoxValue::NIL,
            Literal::BOOLEAN(b) => LoxValue::BOOLEAN(*b),
            Literal::NUMBER(n) => LoxValue::NUMBER(*n),
            Literal::STRING(s) => LoxValue::STRING(s.clone())
        })
    }

    fn evaluate_logical(&mut self, l: &Logical) -> Result<LoxValue, String> {
        let left = self.evaluate(&l.left)?;
        if l.operator.token_type == TokenType::OR {
            if left.to_native_boolean() {
                return Ok(left);
            }
        } else {
            if !left.to_native_boolean() {
                return Ok(left);
            }
        }

        self.evaluate(&l.right)
    }

    fn evaluate_binary(&mut self, b: &Binary) -> Result<LoxValue, String> {
        let left_value = self.evaluate(&b.left)?;
        let right_value = self.evaluate(&b.right)?;
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

    fn evaluate_grouping(&mut self, g: &Grouping) -> Result<LoxValue, String> {
        self.evaluate(&g.expression)
    }

    fn evaluate_unary(&mut self, u: &Unary) -> Result<LoxValue, String> {
        let right_value = self.evaluate(&u.right)?;
        match u.operator.token_type {
            TokenType::MINUS => negate_value(right_value),
            TokenType::BANG => logical_negate_value(right_value),
            _ => {
                panic!("unreachable code");
            }
        }
    }

    fn evaluate_function_call(&mut self, c: &Call) -> Result<LoxValue, String> {
        let callee = self.evaluate(&c.callee)?;
        let mut arguments = Vec::new();
        for a in &c.arguments {
            arguments.push(self.evaluate(a)?);
        }

        if let LoxValue::FUNCTION(function) = callee {
            if arguments.len() != function.arity() {
                Err(format!("Expected {} arguments but got {}.", function.arity(), arguments.len()))
            } else {
                function.call(self, arguments)
            }
        } else {
            Err("Can only call functions and classes.".to_string())
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
    use crate::parser::Parser;

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
        let test_data = &[("13 + 87;", LoxValue::NUMBER(100.0)), 
            ("-(3 + 2) / 2;", LoxValue::NUMBER(-2.5)),
            ("!(3 >= 2) == true;", LoxValue::BOOLEAN(false)),
            ("2;", LoxValue::NUMBER(2.0))
        ];

        for (original, expected) in test_data {
            let original_tokens = scan_tokens(&original);
            let mut parser = Parser::new(original_tokens);
            let stmt = &parser.parse().unwrap()[0];
            let mut interpreter = Interpreter::new();
            let actual = interpreter.evaluate_statement(stmt);

            assert_eq!(actual.unwrap(), *expected, "Value does not match for: {}", original);
        }
    }

    #[test]
    fn test_script() {
        let script = "var a = 1; a = 3; print a; { var a = 2; }";
        let expected_values = &[LoxValue::NIL, LoxValue::NUMBER(3.0), LoxValue::NUMBER(3.0)];
        let original_tokens = scan_tokens(&script);
        let mut parser = Parser::new(original_tokens);
        let stmts = &parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
        for (stmt, expected_value) in stmts.iter().zip(expected_values) {
            assert_eq!(interpreter.evaluate_statement(stmt).unwrap(), *expected_value);
        }
    }
}
