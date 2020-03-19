use std::option::Option;

use crate::scanning::TokenType;
use crate::scanning::Token;

// FIXME is this an anti-pattern?
// I do not want unnamed parameters for the enum parameters...
enum Expr {
    BINARY(Binary),
    GROUPING(Grouping),
    LITERAL(Literal),
    UNARY(Unary)
}

struct Binary {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>
}


struct Grouping {
    expression: Box<Expr>
}

enum Literal {
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64)
}

struct Unary {
    operator: Token,
    right: Box<Expr>
}

fn parenthesize(name: &str, expressions: &[&Expr]) -> String {
    "".to_string()
}

fn print_ast(expr: Expr) -> String {
    "".to_string()
}

impl TokenType {
    fn to_literal(self) -> Option<Literal> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parenthesize() {
    }
}
