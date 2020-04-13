use crate::expr::Expr;
use crate::parser::Statement;
use crate::eval::evaluate;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { }
    }

    pub fn interpret(&self, stmts: &Vec<Statement>) {
        for stmt in stmts {
            self.evaluate_statement(stmt)
        }
    }

    fn evaluate_statement(&self, stmt: &Statement) {
        match stmt {
            Statement::PRINT(expr) => {
                let result = evaluate(expr);
                println!("{:?}", result);
            },
            Statement::EXPRESSION(expr) => {
                let result = evaluate(expr);
            }
        }
    }
}
