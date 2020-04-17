use std::option::Option;

use crate::scanning::TokenType;
use crate::scanning::Token;

// FIXME is this an anti-pattern?
// I do not want unnamed parameters for the enum parameters...
#[derive(Debug)]
pub enum Expr {
    BINARY(Binary),
    GROUPING(Grouping),
    LITERAL(Literal),
    LOGICAL(Logical),
    UNARY(Unary),
    VARIABLE(Variable),
    ASSIGNMENT(Assignment)
}

#[derive(Debug)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>
}

#[derive(Debug)]
pub struct Grouping {
    pub expression: Box<Expr>
}

#[derive(Debug)]
pub enum Literal {
    STRING(String),
    NUMBER(f64),
    BOOLEAN(bool),
    NIL
}

#[derive(Debug)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>
}

#[derive(Debug)]
pub struct Variable {
    pub name: String
}

#[derive(Debug)]
pub struct Assignment {
    pub name: String,
    pub value: Box<Expr>
}

impl TokenType {
    pub fn to_literal(&self) -> Option<Literal> {
        match self {
            TokenType::TRUE => Some(Literal::BOOLEAN(true)),
            TokenType::FALSE => Some(Literal::BOOLEAN(false)),
            TokenType::NUMBER(n) => Some(Literal::NUMBER(*n)),
            TokenType::STRING(s) => Some(Literal::STRING(s.clone())),
            TokenType::NIL => Some(Literal::NIL),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Literal {
        pub fn to_string(&self) -> String {
            match self {
                Literal::STRING(s) => format!("\"{}\"", s),
                Literal::NUMBER(n) => format!("{}", n),
                Literal::BOOLEAN(b) => format!("{}", b),
                Literal::NIL => "nil".to_string()
            }
        }
    }

    impl Expr {
        pub fn print_ast(&self) -> String {
            let mut result = String::new();
            match self {
                Expr::BINARY(b) => {
                    result.push('(');
                    result.push_str(&b.operator.lexeme);
                    result.push(' ');
                    result.push_str(&b.left.print_ast());
                    result.push(' ');
                    result.push_str(&b.right.print_ast());
                    result.push(')');
                },
                Expr::GROUPING(g) => {
                    result.push('(');
                    result.push_str(&"group");
                    result.push(' ');
                    result.push_str(&g.expression.print_ast());
                    result.push(')');
                },
                Expr::LITERAL(l) => {
                    result.push_str(&l.to_string());
                },
                Expr::LOGICAL(l) => {
                    result.push('(');
                    result.push_str(&l.operator.lexeme);
                    result.push(' ');
                    result.push_str(&l.left.print_ast());
                    result.push(' ');
                    result.push_str(&l.right.print_ast());
                    result.push(')');
                },
                Expr::UNARY(u) => {
                    result.push('(');
                    result.push_str(&u.operator.lexeme);
                    result.push(' ');
                    result.push_str(&u.right.print_ast());
                    result.push(')');
                }
                Expr::VARIABLE(v) => {
                    result.push('(');
                    result.push_str(&v.name);
                    result.push(')');
                },
                Expr::ASSIGNMENT(a) => {
                    result.push('(');
                    result.push_str("assignment");
                    result.push(' ');
                    result.push_str(&a.name);
                    result.push(' ');
                    result.push_str(&a.value.print_ast());
                    result.push(')');
                }
            }
            result
        }
    }


    #[test]
    fn test_parenthesize() {
        assert_eq!("(* (- 123) (group 45.67))".to_string(),
            Expr::BINARY(Binary {
                left: Box::new(Expr::UNARY(Unary {
                    operator: Token {lexeme: "-".to_string(), line: 0, token_type: TokenType::MINUS},
                    right: Box::new(Expr::LITERAL(Literal::NUMBER(123.0)))
                })),
                operator: Token {lexeme: "*".to_string(), line: 0, token_type: TokenType::STAR},
                right: Box::new(Expr::GROUPING(Grouping {
                    expression: Box::new(Expr::LITERAL(Literal::NUMBER(45.67)))
                }))
            }).print_ast()
        )
    }
}
