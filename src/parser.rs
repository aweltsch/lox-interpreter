use std::collections::VecDeque;
use std::result::Result;
use std::option::Option;

use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::expr::Expr;
use crate::expr::Binary;
use crate::expr::Literal;
use crate::expr::Grouping;
use crate::expr::Unary;
use crate::expr::Variable;

pub type ParseError = String;

pub struct Parser {
    tokens: VecDeque<Token>
}

impl Parser {
    // FIXME introduce abstraction layer so we dont consume the tokens
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens: VecDeque::from(tokens) }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut token_deque = &mut self.tokens;
        let mut statements = Vec::new();
        while token_deque.len() > 0 && !next_token_matches(&token_deque, &[TokenType::EOF]) {
            if let Ok(stmt) = declaration(&mut token_deque) {
                statements.push(stmt);
            }
            // TODO handle errors?
        }
        return Ok(statements);
    }
}

pub enum Statement {
    PRINT(Expr), EXPRESSION(Expr), VAR(String, Expr)
}

fn declaration(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let result = if next_token_matches(tokens, &[TokenType::VAR]) {
        var_decl(tokens)
    } else {
        statement(tokens)
    };
    if result.is_err() {
        synchronize(tokens);
    }
    return result;
}

fn var_decl(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    if let Some(token) = tokens.pop_front() {
        if let TokenType::IDENTIFIER(name) = token.token_type {
            let initializer = if next_token_matches(tokens, &[TokenType::EQUAL]) {
                expression(tokens)?
            } else {
                Expr::LITERAL(Literal::NIL)
            };
            consume(tokens, TokenType::SEMICOLON).map(|_| Statement::VAR(name, initializer)).ok_or("Expect ';' after variable declaration.".to_string())
        } else {
            Err("Expect variable name.".to_string())
        }
    } else {
        panic!("Programming error, var_decl has been called without any more tokens.");
    }
}

fn statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let is_print_statement = next_token_matches(tokens, &[TokenType::PRINT]);
    let stmt = if is_print_statement {
        tokens.pop_front();
        print_statement(tokens)?
    } else {
        expression_statement(tokens)?
    };

    Ok(stmt)
}

fn print_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::SEMICOLON).map(|_| Statement::PRINT(expr)).ok_or("Expect ';' after expression.".to_string())
}

fn expression_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::SEMICOLON).map(|_| Statement::EXPRESSION(expr)).ok_or("Expect ';' after expression.".to_string())
}

fn consume(tokens: &mut VecDeque<Token>, token_type: TokenType) -> Option<()> {
    if next_token_matches(tokens, &[token_type]) {
        tokens.pop_front();
        Some(())
    } else {
        None
    }
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
        } else if let TokenType::IDENTIFIER(name) = &token.token_type {
            return Ok(Expr::VARIABLE(Variable { name: name.to_string() }));
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
    // FIXME, maybe this is not such a great idea after all...
    impl Statement {
        fn print_ast(&self) -> String {
            match self {
                Statement::PRINT(expr) => expr.print_ast(),
                Statement::EXPRESSION(expr) => expr.print_ast(),
                Statement::VAR(name, initializer) => initializer.print_ast()
            }
        }
        pub fn get_expr(&self) -> &Expr {
            match self {
                Statement::PRINT(expr) => expr,
                Statement::EXPRESSION(expr) => expr,
                Statement::VAR(_, initializer) => initializer
            }
        }
    }

    use super::*;
    use crate::scanning::scan_tokens;

    #[test]
    fn that_ast_is_correct() {
        ast_matches("123 + 17", "(+ 123 17)");
        ast_matches("1 + 2 * 3 == 7", "(== (+ 1 (* 2 3)) 7)");
    }

    fn ast_matches(input: &str, expected: &str) {
        let mut tokens = VecDeque::from(scan_tokens(input));
        let expr = expression(&mut tokens).unwrap();
        let calculated_ast = expr.print_ast();
        assert_eq!(calculated_ast, expected);
    }

    #[test]
    fn report_errors() {
        let strings = &["(123 + 456", "12 +", "(123 <= 123) == (233 * 3) =="];
        for input in strings {
            let tokens = scan_tokens(&input);
            let expr = expression(&mut VecDeque::from(tokens));
            assert!(expr.is_err(), "No error for string: {}", input);
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
