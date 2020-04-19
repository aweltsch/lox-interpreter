use std::collections::VecDeque;
use std::result::Result;
use std::option::Option;

use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::expr::Expr;
use crate::expr::Assignment;
use crate::expr::Binary;
use crate::expr::Literal;
use crate::expr::Logical;
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
            let stmt = declaration(&mut token_deque)?;
            statements.push(stmt);
        }
        return Ok(statements);
    }
}

#[derive(Debug)]
pub enum Statement {
    BLOCK(Vec<Box<Statement>>), PRINT(Expr), EXPRESSION(Expr), VAR(String, Expr),
    IF(IfStatement), WHILE(Expr, Box<Statement>)
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>
}

fn declaration(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let result = if next_token_matches(tokens, &[TokenType::VAR]) {
        tokens.pop_front();
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
                tokens.pop_front();
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
    let stmt = if next_token_matches(tokens, &[TokenType::PRINT]) {
        tokens.pop_front();
        print_statement(tokens)?
    } else if next_token_matches(tokens, &[TokenType::LEFT_BRACE]) {
        block(tokens)?
    } else if next_token_matches(tokens, &[TokenType::IF]) {
        if_statement(tokens)?
    } else if next_token_matches(tokens, &[TokenType::WHILE]) {
        while_statement(tokens)?
    } else if next_token_matches(tokens, &[TokenType::FOR]) {
        for_statement(tokens)?
    } else {
        expression_statement(tokens)?
    };

    Ok(stmt)
}

fn block(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    assert_eq!(tokens.pop_front().unwrap().token_type, TokenType::LEFT_BRACE);
    let mut stmts = Vec::new();
    while tokens.len() > 0 && !next_token_matches(tokens, &[TokenType::RIGHT_BRACE]) {
        stmts.push(Box::new(declaration(tokens)?));
    }

    if next_token_matches(tokens, &[TokenType::RIGHT_BRACE]) {
        tokens.pop_front();
        Ok(Statement::BLOCK(stmts))
    } else {
        Err("Expect '}' after block.".to_string())
    }
}

fn print_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    let expr = expression(tokens)?;
    consume(tokens, TokenType::SEMICOLON).map(|_| Statement::PRINT(expr)).ok_or("Expect ';' after expression.".to_string())
}

fn if_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    assert_eq!(tokens.pop_front().unwrap().token_type, TokenType::IF);
    consume(tokens, TokenType::LEFT_PAREN).ok_or("Expect '(' after if.".to_string())?;
    let condition = expression(tokens)?;
    consume(tokens, TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;

    let then_branch = Box::new(statement(tokens)?);
    let else_branch = if next_token_matches(tokens, &[TokenType::ELSE]) {
        tokens.pop_front();
        Some(Box::new(statement(tokens)?))
    } else {
        None
    };

    Ok(Statement::IF(IfStatement { condition: condition, else_branch: else_branch, then_branch: then_branch }))
}

fn while_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    assert_eq!(tokens.pop_front().unwrap().token_type, TokenType::WHILE);
    consume(tokens, TokenType::LEFT_PAREN).ok_or("Expect '(' after while.".to_string())?;
    let condition = expression(tokens)?;
    consume(tokens, TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;
    let body = statement(tokens)?;
    Ok(Statement::WHILE(condition, Box::new(body)))
}

fn for_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, ParseError> {
    assert_eq!(tokens.pop_front().unwrap().token_type, TokenType::FOR);
    consume(tokens, TokenType::LEFT_PAREN).ok_or("Expect '(' after 'for'.".to_string())?;
    let initializer = if next_token_matches(tokens, &[TokenType::VAR]) {
        tokens.pop_front(); // FIXME nasty
        Some(var_decl(tokens)?)
    } else if next_token_matches(tokens, &[TokenType::SEMICOLON]) {
        tokens.pop_front();
        None
    } else {
        Some(expression_statement(tokens)?)
    };
    let condition = if next_token_matches(tokens, &[TokenType::SEMICOLON]) {
        tokens.pop_front();
        Expr::LITERAL(Literal::BOOLEAN(true))
    } else {
        expression(tokens)?
    };
    consume(tokens, TokenType::SEMICOLON).ok_or("Expect ';' after loop condition.".to_string());

    let increment = if next_token_matches(tokens, &[TokenType::SEMICOLON]) {
        tokens.pop_front();
        None
    } else {
        Some(expression(tokens)?)
    };

    consume(tokens, TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;
    let for_body = statement(tokens)?;
    let mut for_statements = vec![Box::new(for_body)];
    if let Some(expr) = increment {
        for_statements.push(Box::new(Statement::EXPRESSION(expr)));
    }

    let body = Statement::WHILE(condition, Box::new(Statement::BLOCK(for_statements)));
    if let Some(init_stmt) = initializer {
        let stmts = vec![Box::new(init_stmt), Box::new(body)];
        Ok(Statement::BLOCK(stmts))
    } else {
        Ok(body)
    }
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
    assignment(tokens)
}

fn assignment(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let expr = logic_or(tokens);
    if next_token_matches(tokens, &[TokenType::EQUAL]) {
        tokens.pop_front(); // consume "="
        let value = assignment(tokens);
        if let Ok(Expr::VARIABLE(v)) = &expr {
            return Ok(Expr::ASSIGNMENT(Assignment { name: v.name.clone(), value: Box::new(value?) }));
        } else {
            return Err("Invalid assignment target.".to_string());
        }
    }
    return expr;
}

fn logic_or(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = logic_and(tokens)?;
    while next_token_matches(tokens, &[TokenType::OR]) {
        let operator = tokens.pop_front().unwrap();
        let right = logic_and(tokens)?;
        expr = Expr::LOGICAL(Logical { left: Box::new(expr), operator: operator, right: Box::new(right) });

    }

    return Ok(expr);
}

fn logic_and(tokens: &mut VecDeque<Token>) -> Result<Expr, ParseError> {
    let mut expr = equality(tokens)?;
    while next_token_matches(tokens, &[TokenType::AND]) {
        let operator = tokens.pop_front().unwrap();
        let right = equality(tokens)?;
        expr = Expr::LOGICAL(Logical { left: Box::new(expr), operator: operator, right: Box::new(right) });

    }

    return Ok(expr);
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
    if let Some(token) = tokens.pop_front() {
        if let Some(result) = token.token_type.to_literal() {
            return Ok(Expr::LITERAL(result));
        } else if let TokenType::IDENTIFIER(name) = token.token_type {
            return Ok(Expr::VARIABLE(Variable { name: name }));
        } else if let TokenType::LEFT_PAREN = token.token_type {
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
