use crate::expr::Expr;
use crate::parser::Statement;
use crate::parser::ParseError;
use crate::eval::evaluate;
use crate::eval::LoxValue;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { }
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
