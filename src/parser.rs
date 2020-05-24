use std::collections::VecDeque;
use std::result::Result;
use std::option::Option;
use std::fmt;
use std::rc::Rc;

use crate::scanning::Token;
use crate::scanning::TokenType;
use crate::expr::Expr;
use crate::expr::Assignment;
use crate::expr::Binary;
use crate::expr::Call;
use crate::expr::Literal;
use crate::expr::Logical;
use crate::expr::Grouping;
use crate::expr::Unary;
use crate::expr::Variable;

pub type ParseError = String;

pub struct Parser {
    tokens: VecDeque<Token>
}

#[derive(Debug)]
pub enum Statement {
    BLOCK(Vec<Box<Statement>>), PRINT(Expr), EXPRESSION(Expr), VAR(String, Expr),
    IF(IfStatement), WHILE(Expr, Box<Statement>), FUNCTION(Rc<FunctionDeclaration>)
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Box<Statement>>
}

pub enum FunctionKind {
    FUNCTION
}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = match self {
            FunctionKind::FUNCTION => "function"
        };
        write!(f, "{}", output)
    }
}

impl Parser {
    // FIXME introduce abstraction layer so we dont consume the tokens
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens: VecDeque::from(tokens) }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();
        while self.tokens.len() > 0 && !next_token_matches_any(&self.tokens, &[TokenType::EOF]) {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        return Ok(statements);
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let result = if next_token_matches_any(&self.tokens, &[TokenType::FUN]) {
            self.tokens.pop_front();
            self.fun_decl()
        } else if next_token_matches_any(&self.tokens, &[TokenType::VAR]) {
            self.tokens.pop_front();
            self.var_decl()
        } else {
            self.statement()
        };
        if result.is_err() {
            self.synchronize();
        }
        return result;
    }

    fn fun_decl(&mut self) -> Result<Statement, ParseError> {
        self.function(FunctionKind::FUNCTION)
    }

    fn function(&mut self, kind: FunctionKind) -> Result<Statement, ParseError> {
        let name = match self.tokens.pop_front() {
            Some(Token {token_type: TokenType::IDENTIFIER(name), lexeme: lexeme, line: line}) => Ok(Token {token_type: TokenType::IDENTIFIER(name), lexeme: lexeme, line: line}),
            _ => Err(format!("Expect {} name.", kind))
        }?;
        self.consume(TokenType::LEFT_PAREN).ok_or(format!("Expect '(' after {} name.", kind))?;

        let mut parameters = Vec::new();
        if !next_token_matches_any(&self.tokens, &[TokenType::RIGHT_PAREN]) {
            loop {
                if parameters.len() >= 255 {
                    return Err("Cannot have more than 255 parameters.".to_string());
                }

                // FIXME this construct is awkward!
                let token = match self.tokens.pop_front() {
                    Some(Token {token_type: TokenType::IDENTIFIER(name), lexeme: lexeme, line: line}) => 
                        Ok(Token {token_type: TokenType::IDENTIFIER(name), lexeme: lexeme, line: line}),
                    _ => Err(format!("Expect parameter name."))
                }?;
                parameters.push(token);

                if !next_token_matches_any(&self.tokens, &[TokenType::COMMA]) {
                    break;
                }
                self.tokens.pop_front();
            }
        }
        self.consume(TokenType::RIGHT_PAREN).ok_or("Expect ')' after parameters.".to_string())?;
        let block = self.block()?;
        if let Statement::BLOCK(body) = block {
            Ok(Statement::FUNCTION(Rc::new(FunctionDeclaration {name: name, params: parameters, body: body})))
        } else {
            panic!("programming error, block function should return a block statement");
        }
    }

    fn var_decl(&mut self) -> Result<Statement, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            if let TokenType::IDENTIFIER(name) = token.token_type {
                let initializer = if next_token_matches_any(&self.tokens, &[TokenType::EQUAL]) {
                    self.tokens.pop_front();
                    self.expression()?
                } else {
                    Expr::LITERAL(Literal::NIL)
                };
                self.consume(TokenType::SEMICOLON).map(|_| Statement::VAR(name, initializer)).ok_or("Expect ';' after variable declaration.".to_string())
            } else {
                Err("Expect variable name.".to_string())
            }
        } else {
            panic!("Programming error, var_decl has been called without any more tokens.");
        }
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        let stmt = if next_token_matches_any(&self.tokens, &[TokenType::PRINT]) {
            self.tokens.pop_front();
            self.print_statement()?
        } else if next_token_matches_any(&self.tokens, &[TokenType::LEFT_BRACE]) {
            self.block()?
        } else if next_token_matches_any(&self.tokens, &[TokenType::IF]) {
            self.if_statement()?
        } else if next_token_matches_any(&self.tokens, &[TokenType::WHILE]) {
            self.while_statement()?
        } else if next_token_matches_any(&self.tokens, &[TokenType::FOR]) {
            self.for_statement()?
        } else {
            self.expression_statement()?
        };

        Ok(stmt)
    }

    fn block(&mut self) -> Result<Statement, ParseError> {
        assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::LEFT_BRACE);
        let mut stmts = Vec::new();
        while self.tokens.len() > 0 && !next_token_matches_any(&self.tokens, &[TokenType::RIGHT_BRACE]) {
            stmts.push(Box::new(self.declaration()?));
        }

        if next_token_matches_any(&self.tokens, &[TokenType::RIGHT_BRACE]) {
            self.tokens.pop_front();
            Ok(Statement::BLOCK(stmts))
        } else {
            Err("Expect '}' after block.".to_string())
        }
    }

    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SEMICOLON).map(|_| Statement::PRINT(expr)).ok_or("Expect ';' after expression.".to_string())
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::IF);
        self.consume(TokenType::LEFT_PAREN).ok_or("Expect '(' after if.".to_string())?;
        let condition = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = if next_token_matches_any(&self.tokens, &[TokenType::ELSE]) {
            self.tokens.pop_front();
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Statement::IF(IfStatement { condition: condition, else_branch: else_branch, then_branch: then_branch }))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::WHILE);
        self.consume(TokenType::LEFT_PAREN).ok_or("Expect '(' after while.".to_string())?;
        let condition = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;
        let body = self.statement()?;
        Ok(Statement::WHILE(condition, Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::FOR);
        self.consume(TokenType::LEFT_PAREN).ok_or("Expect '(' after 'for'.".to_string())?;
        let initializer = if next_token_matches_any(&self.tokens, &[TokenType::VAR]) {
            self.tokens.pop_front(); // FIXME nasty
            Some(self.var_decl()?)
        } else if next_token_matches_any(&self.tokens, &[TokenType::SEMICOLON]) {
            self.tokens.pop_front();
            None
        } else {
            Some(self.expression_statement()?)
        };
        let condition = if next_token_matches_any(&self.tokens, &[TokenType::SEMICOLON]) {
            self.tokens.pop_front();
            Expr::LITERAL(Literal::BOOLEAN(true))
        } else {
            self.expression()?
        };
        self.consume(TokenType::SEMICOLON).ok_or("Expect ';' after loop condition.".to_string());

        let increment = if next_token_matches_any(&self.tokens, &[TokenType::SEMICOLON]) {
            self.tokens.pop_front();
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::RIGHT_PAREN).ok_or("Expect ')' after if condition.".to_string())?;
        let for_body = self.statement()?;
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

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SEMICOLON).map(|_| Statement::EXPRESSION(expr)).ok_or("Expect ';' after expression.".to_string())
    }

    fn consume(&mut self, token_type: TokenType) -> Option<Token> {
        if next_token_matches_any(&self.tokens, &[token_type]) {
            self.tokens.pop_front()
        } else {
            None
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logic_or();
        if next_token_matches_any(&self.tokens, &[TokenType::EQUAL]) {
            self.tokens.pop_front(); // consume "="
            let value = self.assignment();
            if let Ok(Expr::VARIABLE(v)) = &expr {
                return Ok(Expr::ASSIGNMENT(Assignment { name: v.name.clone(), value: Box::new(value?) }));
            } else {
                return Err("Invalid assignment target.".to_string());
            }
        }
        return expr;
    }

    fn logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logic_and()?;
        while next_token_matches_any(&self.tokens, &[TokenType::OR]) {
            let operator = self.tokens.pop_front().unwrap();
            let right = self.logic_and()?;
            expr = Expr::LOGICAL(Logical { left: Box::new(expr), operator: operator, right: Box::new(right) });

        }

        return Ok(expr);
    }

    fn logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while next_token_matches_any(&self.tokens, &[TokenType::AND]) {
            let operator = self.tokens.pop_front().unwrap();
            let right = self.equality()?;
            expr = Expr::LOGICAL(Logical { left: Box::new(expr), operator: operator, right: Box::new(right) });

        }

        return Ok(expr);
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while next_token_matches_any(&self.tokens, &[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL]) {
            if let Some(operator) = self.tokens.pop_front() {
                let right = self.comparison()?;
                expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.addition()?;
        while next_token_matches_any(&self.tokens, &[TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL]) {
            if let Some(operator) = self.tokens.pop_front() {
                let right = self.addition()?;
                expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
            }
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;

        while next_token_matches_any(&self.tokens, &[TokenType::PLUS, TokenType::MINUS]) {
            if let Some(operator) = self.tokens.pop_front() {
                let right = self.multiplication()?;
                expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while next_token_matches_any(&self.tokens, &[TokenType::STAR, TokenType::SLASH]) {
            if let Some(operator) = self.tokens.pop_front() {
                let right = self.unary()?;
                expr = Expr::BINARY(Binary {left: Box::new(expr), operator: operator, right: Box::new(right)});
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if next_token_matches_any(&self.tokens, &[TokenType::BANG, TokenType::MINUS]) {
            if let Some(operator) = self.tokens.pop_front() {
                let right = self.unary()?;
                return Ok(Expr::UNARY(Unary {operator: operator, right: Box::new(right)}))
            }
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if next_token_matches_any(&self.tokens, &[TokenType::LEFT_PAREN]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::LEFT_PAREN);
        let mut arguments = Vec::new();
        if !next_token_matches_any(&self.tokens, &[TokenType::RIGHT_PAREN]) {
            loop {
                // NOTE: we do not introduce a maximum arguments size here!
                arguments.push(self.expression()?);
                if !next_token_matches_any(&self.tokens, &[TokenType::COMMA]) {
                    break;
                }
                assert_eq!(self.tokens.pop_front().unwrap().token_type, TokenType::COMMA);
            }
        }

        let paren = self.consume(TokenType::RIGHT_PAREN).ok_or("Expect ')' after expression.".to_string())?;

        Ok(Expr::CALL(Call {callee: Box::new(expr), paren: paren, arguments: arguments}))
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            if let Some(result) = token.token_type.to_literal() {
                return Ok(Expr::LITERAL(result));
            } else if let TokenType::IDENTIFIER(name) = token.token_type {
                return Ok(Expr::VARIABLE(Variable { name: name }));
            } else if let TokenType::LEFT_PAREN = token.token_type {
                let expr = self.expression()?;

                if let Some(other_token) = self.tokens.pop_front() {
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

    fn synchronize(&mut self) {
        while self.tokens.len() > 0 {
            if next_token_matches_any(&self.tokens,
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

            let token = self.tokens.pop_front();
            if token.map_or(false, |t| t.token_type == TokenType::SEMICOLON) {
                return;
            }
        }
    }
}

// FIXME this will not work for TokenType with values i.e. TokenType::NUMBER
fn next_token_matches_any(tokens: &VecDeque<Token>, expected: &[TokenType]) -> bool {
    tokens.get(0).map_or(false, |token| {
        expected.contains(&token.token_type)
    })
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
        let mut parser = Parser::new(tokens);
        let expr = parser.expression().unwrap();
        let calculated_ast = expr.print_ast();
        assert_eq!(calculated_ast, expected);
    }

    #[test]
    fn report_errors() {
        let strings = &["(123 + 456", "12 +", "(123 <= 123) == (233 * 3) =="];
        for input in strings {
            let tokens = scan_tokens(&input);
            let mut parser = Parser::new(tokens);
            let expr = parser.expression();
            assert!(expr.is_err(), "No error for string: {}", input);
        }
    }

    #[test]
    fn synchronize_discards_elements() {
        let original = &["asdf; 123 + 345", "fun something()", "what fun something()", "; class what()", "a statement();"];
        let expected = &["123 + 345", "fun something()", "fun something()", "class what()", ""];
        for (original, expected) in original.iter().zip(expected.iter()) {
            let mut parser = Parser::new(scan_tokens(&original));
            let expected_tokens = VecDeque::from(scan_tokens(&expected));

            parser.synchronize();
            assert_eq!(parser.tokens, expected_tokens, "Tokens don't match for: {}", original);
        }
    }
}
