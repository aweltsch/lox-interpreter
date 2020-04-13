use std::collections::HashMap;

use crate::parser::Statement;
use crate::parser::ParseError;
use crate::eval::evaluate;
use crate::eval::LoxValue;

pub struct Interpreter {
    environment: Environment
}

pub struct Environment {
    variable_map: HashMap<String, LoxValue>
}

impl Environment {
    pub fn new() -> Self {
        Environment { variable_map: HashMap::new() }
    }

    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.variable_map.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<&LoxValue, String> {
        self.variable_map.get(name).ok_or_else(|| format!("Undefined variable {}.", name))
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { environment: Environment::new() }
    }

    pub fn interpret(&self, stmts: &Vec<Statement>) {
        for stmt in stmts {
            // TODO report error!
            let result = self.evaluate_statement(stmt);
            if let Err(s) = result {
                panic!("Error {}", s);
            }
        }
    }

    fn evaluate_statement(&self, stmt: &Statement) -> Result<LoxValue, ParseError> {
        match stmt {
            Statement::PRINT(expr) => {
                let result = evaluate(expr)?;
                println!("{}", result);
                Ok(result)
            },
            Statement::EXPRESSION(expr) => {
                evaluate(expr)
            },
            Statement::VAR(name, initializer) => {
                panic!("not yet implemented!");
            }
        }
    }
}
