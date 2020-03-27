use std::collections::VecDeque;
use std::result::Result;
use std::option::Option;

use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::expr::Expr;
use crate::expr::Binary;
use crate::expr::Grouping;
use crate::expr::Unary;

type ParseError = String;

pub enum Statement {
    PRINT(Expr), EXPRESSION(Expr)
}

// FIXME don't want to heap allocate...
// returns AST
// consumes tokens!
pub fn parse(tokens: Vec<Token>) -> Option<Expr> {
    match expression(&mut VecDeque::from(tokens)) {
        Ok(expr) => Some(expr),
        Err(why) => {
            eprintln!("{}", why);
            None
        }
    }
}

fn statement(tokens: Vec<Token>) -> Result<Statement, ParseError> {
    Err("".to_string())
}

fn expression(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    equality(tokens)
}

fn equality(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = comparison(tokens)?;

    while next_token_matches(tokens, &[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL]) {
        if let Some(operator) = tokens.pop_front() {
            let right = comparison(tokens)?;
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }

    Ok(expr)
}

fn comparison(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = addition(tokens)?;
    while next_token_matches(tokens, &[TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL]) {
        if let Some(operator) = tokens.pop_front() {
            let right = addition(tokens)?;
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    Ok(expr)
}

fn addition(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = multiplication(tokens)?;

    while next_token_matches(tokens, &[TokenType::PLUS, TokenType::MINUS]) {
        if let Some(operator) = tokens.pop_front() {
            let right = multiplication(tokens)?;
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    Ok(expr)
}

fn multiplication(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = unary(tokens)?;

    while next_token_matches(tokens, &[TokenType::STAR, TokenType::SLASH]) {
        if let Some(operator) = tokens.pop_front() {
            let right = unary(tokens)?;
            expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
        }
    }
    Ok(expr)
}

fn unary(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    if next_token_matches(tokens, &[TokenType::BANG, TokenType::MINUS]) {
        if let Some(operator) = tokens.pop_front() {
            let right = unary(tokens)?;
            return Ok(Expr::UNARY(Unary {operator: operator, right: Box::new(right)}))
        }
    }
    primary(tokens)
}

fn primary(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    if let Some(token) = tokens.get(0) {
        if let Some(result) = token.token_type.to_literal() {
            tokens.pop_front(); // consume
            return Ok(Expr::LITERAL(result));
        } else if let TokenType::LEFT_PAREN = token.token_type {
            tokens.pop_front(); // consume
            let expr = expression(tokens)?;

            if let Some(other_token) = tokens.pop_front() {
                if other_token.token_type != TokenType::RIGHT_PAREN {
                    return Err("Expect ')' after expression.".to_string());
                }
            }
            return Ok(
                Expr::GROUPING(Grouping {expression: Box::new(expr)})
                );
        }
    }
    Err("Could not match Token".to_string())
}

// FIXME this will not work for TokenType with values i.e. TokenType::NUMBER
fn next_token_matches(tokens: &VecDeque<Token>, expected: &[TokenType]) -> bool {
    tokens.get(0).map_or(false, |token| {
        expected.contains(&token.token_type)
    })
}

fn synchronize(tokens: &mut VecDeque<Token>) {
    while tokens.len() > 0 {
        if next_token_matches(tokens,
                              &[TokenType::CLASS,
                              TokenType::FUN,
                              TokenType::VAR,
                              TokenType::FOR,
                              TokenType::IF,
                              TokenType::WHILE,
                              TokenType::PRINT,
                              TokenType::RETURN]) {
            return;
        }

        let token = tokens.pop_front();
        if token.map_or(false, |t| t.token_type == TokenType::SEMICOLON) {
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanning::scan_tokens;

    #[test]
    fn that_ast_is_correct() {
        ast_matches("123 + 17", "(+ 123 17)");
        ast_matches("1 + 2 * 3 == 7", "(== (+ 1 (* 2 3)) 7)");
    }

    fn ast_matches(input: &str, expected: &str) {
        let tokens = scan_tokens(input);
        let expr = parse(tokens);
        let calculated_ast = expr.unwrap().print_ast();
        assert_eq!(calculated_ast, expected);
    }

    #[test]
    fn report_errors() {
        let strings = &["(123 + 456", "12 +", "(123 <= 123) == (233 * 3) =="];
        for input in strings {
            let tokens = scan_tokens(&input);
            let expr = parse(tokens);
            assert!(expr.is_none(), "No error for string: {}", input);
        }
    }

    #[test]
    // FIXME this implementation is convenient, but integrates the scan_tokens component...
    fn synchronize_discards_elements() {
        let original = &["asdf; 123 + 345", "fun something()", "what fun something()", "; class what()", "a statement();"];
        let expected = &["123 + 345", "fun something()", "fun something()", "class what()", ""];
        for (original, expected) in original.iter().zip(expected.iter()) {
            let mut original_tokens = VecDeque::from(scan_tokens(&original));
            let expected_tokens = VecDeque::from(scan_tokens(&expected));

            let _synchronized_tokens = synchronize(&mut original_tokens);
            assert_eq!(original_tokens, expected_tokens, "Tokens don't match for: {}", original);
        }
    }
}
