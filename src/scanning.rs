use std::option::Option;
use crate::n_peekable::NPeekable;

struct ScannerState<'a> {
    line: i32,
    cur_lexeme: String,
    char_iter: NPeekable<'a>
}

impl<'a> ScannerState<'a> {
    fn new(source: &'a str) -> ScannerState {
        ScannerState { line: 0, cur_lexeme: String::new(), char_iter: NPeekable::new(source) }
    }

    fn has_next(&mut self) -> bool {
        self.char_iter.peek().is_some()
    }

    // FIXME the usage of advance might suggest, that we don't need to return anything from here
    // rethink interface
    fn advance(&mut self) -> Option<char> {
        let result = self.char_iter.next();
        self.cur_lexeme.extend(result);
        result
    }

    fn skip_whitespace(&mut  self) {
        while self.char_iter.peek().map_or(false, |c| c.is_whitespace()) {
            let c = self.char_iter.next().unwrap();
            if c == '\n' {
                self.line += 1;
            }
        }
    }

    fn scan_token(&mut self) -> Option<TokenType> {
        self.skip_whitespace();

        let a = self.char_iter.peek()?;
        assert!(!a.is_whitespace());

        let token_type = if a.is_digit(10) {
            self.scan_number()
        } else if a.is_alphabetic() {
            self.scan_identifier()
        } else if *a == '"' {
            self.read_string()
        } else {
            // FIXME too deeply nested here
            let c = self.advance()?;
            match c {
                '(' => Some(TokenType::LEFT_PAREN),
                ')' => Some(TokenType::RIGHT_PAREN),
                '{' => Some(TokenType::LEFT_BRACE),
                '}' => Some(TokenType::RIGHT_BRACE),
                ',' => Some(TokenType::COMMA),
                '.' => Some(TokenType::DOT),
                '-' => Some(TokenType::MINUS),
                '+' => Some(TokenType::PLUS),
                ';' => Some(TokenType::SEMICOLON),
                '*' => Some(TokenType::STAR),
                '/' => if self.next_char_matches('/') {
                    while self.has_next() && !self.next_char_matches('\n') {
                        self.advance();
                    }
                    None
                } else {
                    Some(TokenType::SLASH)
                },
                _ => {
                    if self.next_char_matches('=') {
                        self.advance();
                        match c {
                            '!' => Some(TokenType::BANG_EQUAL),
                            '=' => Some(TokenType::EQUAL_EQUAL),
                            '<' => Some(TokenType::LESS_EQUAL),
                            '>' => Some(TokenType::GREATER_EQUAL),
                            _ => None
                        }
                    } else {
                        match c {
                            '!' => Some(TokenType::BANG),
                            '=' => Some(TokenType::EQUAL),
                            '<' => Some(TokenType::LESS),
                            '>' => Some(TokenType::GREATER),
                            _ => None
                        }
                    }
                }
            }
        };


        if token_type.is_none() {
            error(self.line, "Unexpected character.");
        }
        token_type
    }


    fn read_string(&mut self) -> Option<TokenType> {
        assert!(self.next_char_matches('"'));
        self.advance();

        let mut value = String::new();
        while !self.next_char_matches('"') {
            let x = self.advance()?;
            if x == '\n' {
                self.line += 1;
            }
            value.push(x);
        }

        if self.next_char_matches('"') {
            self.advance(); // skip "
            Some(TokenType::STRING(value))
        } else {
            error(self.line, "Unterminated string.");
            None
        }
    }

    fn scan_number(&mut self) -> Option<TokenType> {
        assert!(self.char_iter.peek().map_or(false, |c| c.is_digit(10)));
        while self.char_iter.peek().map_or(false, |c| c.is_digit(10)) {
            self.advance();
        }

        if self.next_char_matches('.')
            && self.char_iter.n_peek(2).map_or(false, |c| c.is_digit(10)) {
                self.advance();
            }

        while self.char_iter.peek().map_or(false, |c| c.is_digit(10)) {
            self.advance();
        }

        self.cur_lexeme.as_str().parse::<f64>().map(|n| TokenType::NUMBER(n)).ok()
    }

    fn scan_identifier(&mut self) -> Option<TokenType> {
        while self.char_iter.peek().map_or(false, |c| c.is_alphanumeric()) {
            self.advance();
        }
        Some(match self.cur_lexeme.as_str() {
            "and" => TokenType::AND,                       
            "class" => TokenType::CLASS,                     
            "else" => TokenType::ELSE,                      
            "false" => TokenType::FALSE,                     
            "for" => TokenType::FOR,                       
            "fun" => TokenType::FUN,                       
            "if" => TokenType::IF,                        
            "nil" => TokenType::NIL,                       
            "or" => TokenType::OR,                        
            "print" => TokenType::PRINT,                     
            "return" => TokenType::RETURN,                    
            "super" => TokenType::SUPER,                     
            "this" => TokenType::THIS,                      
            "true" => TokenType::TRUE,                      
            "var" => TokenType::VAR,                       
            "while" => TokenType::WHILE,                     
            a => TokenType::IDENTIFIER(a.to_string())
        })
    }

    fn next_char_matches(&mut self, c: char) -> bool {
        self.char_iter.peek().map_or(false, |a| *a == c)
    }
}

pub fn scan_tokens(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut scanner_state = ScannerState::new(source);

    while scanner_state.has_next() {
        // start of a new lexeme
        scanner_state.cur_lexeme.clear();

        let token_type = scanner_state.scan_token();
        tokens.extend(token_type.map(|t| Token {token_type: t, lexeme: scanner_state.cur_lexeme.to_string(), line: scanner_state.line}));
    }
    tokens.push(Token {token_type: TokenType::EOF, lexeme: "".to_string(), line: scanner_state.line});
    tokens
}


// TODO: proper fmt::Display trait
// the literal is bundled in the TokenType
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, // FIXME do we need lexeme?
    pub line: i32
}

fn error(line: i32, message: &str) {
    report(line, "", message);
}

fn report(line: i32, location: &str, message: &str) {
    eprintln!("[line {} ] Error {} : {}", line, location, message);
}

#[derive(Debug)]
#[derive(PartialEq)]
#[allow(non_camel_case_types)]   
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER(String), STRING(String), NUMBER(f64),

    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_correctly_scanned_token(input: &str, expected_token_type: TokenType) {
        let result = scan_tokens(input);
        assert_eq!(2, result.len(), "{}", input);
        assert_eq!(Token { line: 0, lexeme: input.to_string(), token_type:
            expected_token_type }, result[0], "{}", input)
    }

    #[test]
    fn scan_token() {
        assert_correctly_scanned_token("(", TokenType::LEFT_PAREN);
        assert_correctly_scanned_token(")", TokenType::RIGHT_PAREN);
        assert_correctly_scanned_token("{", TokenType::LEFT_BRACE);
        assert_correctly_scanned_token("}", TokenType::RIGHT_BRACE);
        assert_correctly_scanned_token(",", TokenType::COMMA);
        assert_correctly_scanned_token(".", TokenType::DOT);
        assert_correctly_scanned_token("-", TokenType::MINUS);
        assert_correctly_scanned_token("+", TokenType::PLUS);
        assert_correctly_scanned_token(";", TokenType::SEMICOLON);
        assert_correctly_scanned_token("/", TokenType::SLASH);
        assert_correctly_scanned_token("*", TokenType::STAR);
        assert_correctly_scanned_token("!", TokenType::BANG);
        assert_correctly_scanned_token("!=", TokenType::BANG_EQUAL);
        assert_correctly_scanned_token("=", TokenType::EQUAL);
        assert_correctly_scanned_token("==", TokenType::EQUAL_EQUAL);
        assert_correctly_scanned_token(">", TokenType::GREATER);
        assert_correctly_scanned_token(">=", TokenType::GREATER_EQUAL);
        assert_correctly_scanned_token("<", TokenType::LESS);
        assert_correctly_scanned_token("<=", TokenType::LESS_EQUAL);
        assert_correctly_scanned_token("\"string\"", TokenType::STRING("string".to_string()));
        assert_correctly_scanned_token("1", TokenType::NUMBER(1.0));
        assert_correctly_scanned_token("1.23", TokenType::NUMBER(1.23));
        assert_correctly_scanned_token("identifier", TokenType::IDENTIFIER("identifier".to_string()));
        assert_correctly_scanned_token("and", TokenType::AND);
        assert_correctly_scanned_token("class", TokenType::CLASS);
        assert_correctly_scanned_token("else", TokenType::ELSE);
        assert_correctly_scanned_token("false", TokenType::FALSE);
        assert_correctly_scanned_token("for", TokenType::FOR);
        assert_correctly_scanned_token("fun", TokenType::FUN);
        assert_correctly_scanned_token("if", TokenType::IF);
        assert_correctly_scanned_token("nil", TokenType::NIL);
        assert_correctly_scanned_token("or", TokenType::OR);
        assert_correctly_scanned_token("print", TokenType::PRINT);
        assert_correctly_scanned_token("return", TokenType::RETURN);
        assert_correctly_scanned_token("super", TokenType::SUPER);
        assert_correctly_scanned_token("this", TokenType::THIS);
        assert_correctly_scanned_token("true", TokenType::TRUE);
        assert_correctly_scanned_token("var", TokenType::VAR);
        assert_correctly_scanned_token("while", TokenType::WHILE);
    }

    #[test]
    fn scan_number_with_function_call() {
        let tokens = scan_tokens("-123.sqrt()");
        assert_eq!(7, tokens.len());
        assert_eq!(TokenType::MINUS, tokens[0].token_type);
        assert_eq!(TokenType::NUMBER(123.0), tokens[1].token_type);
        assert_eq!(TokenType::DOT, tokens[2].token_type);
        assert_eq!(TokenType::IDENTIFIER("sqrt".to_string()), tokens[3].token_type);
        assert_eq!(TokenType::LEFT_PAREN, tokens[4].token_type);
        assert_eq!(TokenType::RIGHT_PAREN, tokens[5].token_type);
        assert_eq!(TokenType::EOF, tokens[6].token_type);
    }

    #[test]
    fn lox_files_are_scanned_correctly() {
        scanned_file_matches_token_types("test_data/example-0.lox",
                                         vec![TokenType::FUN,
                                         TokenType::IDENTIFIER("someFun".to_string()),
                                         TokenType::LEFT_PAREN,
                                         TokenType::IDENTIFIER("someParam".to_string()),
                                         TokenType::RIGHT_PAREN,
                                         TokenType::LEFT_BRACE,
                                         TokenType::RETURN,
                                         TokenType::NUMBER(0.0),
                                         TokenType::SEMICOLON,
                                         TokenType::RIGHT_BRACE,
                                         TokenType::EOF]
                                        );
        scanned_file_matches_token_types("test_data/example-1.lox",
                                         vec![TokenType::FOR,
                                         TokenType::LEFT_PAREN,
                                         TokenType::VAR,
                                         TokenType::IDENTIFIER("a".to_string()),
                                         TokenType::EQUAL,
                                         TokenType::NUMBER(1.0),
                                         TokenType::SEMICOLON,
                                         TokenType::IDENTIFIER("a".to_string()),
                                         TokenType::LESS,
                                         TokenType::NUMBER(10.0),
                                         TokenType::SEMICOLON,
                                         TokenType::IDENTIFIER("a".to_string()),
                                         TokenType::EQUAL,
                                         TokenType::IDENTIFIER("a".to_string()),
                                         TokenType::PLUS,
                                         TokenType::NUMBER(1.0),
                                         TokenType::RIGHT_PAREN,
                                         TokenType::LEFT_BRACE,
                                         TokenType::PRINT,
                                         TokenType::IDENTIFIER("a".to_string()),
                                         TokenType::SEMICOLON,
                                         TokenType::RIGHT_BRACE,
                                         TokenType::EOF]
                                             );
        scanned_file_matches_token_types("test_data/example-2.lox",
                                         vec![TokenType::CLASS,
                                         TokenType::IDENTIFIER("SomeClass".to_string()),
                                         TokenType::LEFT_BRACE,
                                         TokenType::IDENTIFIER("someMethod".to_string()),
                                         TokenType::LEFT_PAREN,
                                         TokenType::IDENTIFIER("someParam".to_string()),
                                         TokenType::RIGHT_PAREN,
                                         TokenType::LEFT_BRACE,
                                         TokenType::VAR,
                                         TokenType::IDENTIFIER("someVar".to_string()),
                                         TokenType::EQUAL,
                                         TokenType::IDENTIFIER("someParam".to_string()),
                                         TokenType::PLUS,
                                         TokenType::STRING("text'!".to_string()),
                                         TokenType::SEMICOLON,
                                         TokenType::RETURN,
                                         TokenType::IDENTIFIER("someVar".to_string()),
                                         TokenType::SEMICOLON,
                                         TokenType::RIGHT_BRACE,
                                         TokenType::RIGHT_BRACE,
                                         TokenType::EOF]
                                             );
    }

    fn scanned_file_matches_token_types(fileName: &str, expected: Vec<TokenType>) {
        let path = Path::new(fileName);
        match fs::read_to_string(path) {
            Ok(file_content) => {
                let tokens = scan_tokens(&file_content);
                assert_eq!(tokens.len(), expected.len());
                for (token, expected_type) in tokens.iter().zip(expected.iter()) {
                    assert_eq!(token.token_type, *expected_type);
                }
            }
            Err(why) => assert!(false, "{}", why.description())
        }
    }
}
