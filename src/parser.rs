use std::iter::Peekable;
use std::collections::VecDeque;

use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::expr::Expr;
use crate::expr::Binary;
use crate::expr::Grouping;
use crate::expr::Literal;

// returns AST
// consumes tokens!
fn parse(mut tokens: Vec<Token>) -> Expr {
    expression(&mut VecDeque::from(tokens))
}

// FIXME change to vecdeque or peekable iterator!
fn expression(tokens: &mut VecDeque<Token>) -> Expr {
    equality(tokens)
}

fn equality(tokens: &mut VecDeque<Token>) -> Expr {
    let mut expr = comparison(tokens);

    while next_token_matches(tokens, &[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL]) { // tokens[0] == BANG_EQUAL || EQUAL_EQUAL
        if let Some(operator) = tokens.pop_front() {
            let right = comparison(tokens);
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }

    expr
}

fn comparison(tokens: &mut VecDeque<Token>) -> Expr {
    let mut expr = addition(tokens);
    while next_token_matches(tokens, &[TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL]) {
        if let Some(operator) = tokens.pop_front() {
            let right = addition(tokens);
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    expr
}

fn addition(tokens: &mut VecDeque<Token>) -> Expr {
    let mut expr = multiplication(tokens);

    while next_token_matches(tokens, &[TokenType::PLUS, TokenType::MINUS]) {
        if let Some(operator) = tokens.pop_front() {
            let right = multiplication(tokens);
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    expr
}

fn multiplication(tokens: &mut VecDeque<Token>) -> Expr {
    let mut expr = unary(tokens);

    while next_token_matches(tokens, &[TokenType::STAR, TokenType::SLASH]) {
        if let Some(operator) = tokens.pop_front() {
            let right = unary(tokens);
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    expr
}

fn unary(tokens: &mut VecDeque<Token>) -> Expr {
    Expr::LITERAL(Literal::NIL)
}

// FIXME this will not work for TokenType with values i.e. TokenType::NUMBER
fn next_token_matches(tokens: &VecDeque<Token>, expected: &[TokenType]) -> bool {
    if let Some(token) = tokens.get(0) {
        for token_type in expected {
            if token_type == &token.token_type {
            }
        }
    }
    false
}

fn primary(tokens: &mut VecDeque<Token>) -> Expr {
    if let Some(token) = tokens.get(0) {
        if let Some(result) = token.token_type.to_literal() {
            tokens.pop_front(); // consume
            return Expr::LITERAL(result);
        } else if let TokenType::LEFT_PAREN = token.token_type {
            tokens.pop_front(); // consume
            let expr = expression(tokens);

            if let Some(other_token) = tokens.pop_front() {
                if other_token.token_type != TokenType::RIGHT_PAREN {
                    panic!("Expect ')' after expression.");
                }
            }
        }
    }
    Expr::LITERAL(Literal::NIL)
}

#[cfg(test)]
mod tests {
    fn what() {
        assert_eq!(false, true);
    }
}
