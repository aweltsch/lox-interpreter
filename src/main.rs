use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::io;
use std::iter::Peekable;
use std::str::Chars;
use std::option::Option;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        let path = Path::new(&args[1]);
        run_file(path);
    } else if args.len() == 1 {
        run_prompt();
    } else {
        panic!("Fatal: there needs to be at least one argument!");
    }
}

fn run_file(path: &Path) {
    let file_content = match fs::read_to_string(path) {
        Err(why) => panic!("can not read file conents: {}", why.description()),
        Ok(s) => s
    };
    match run(&file_content) {
        Ok(v) => v,
        Err(_) => std::process::exit(65)
    }
}

fn run_prompt() {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Err(why) => panic!("can not read from stdin"),
            Ok(s) => s
        };
        // ignore error while running script
        match run(&line) {
            Ok(v) => v,
            Err(_) => ()
        };
    }
}

fn run(s: &str) -> Result<(), &'static str> {
    let tokens = scan_tokens(s);

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    Ok(())
}

struct ScannerState<'a> {
    line: i32,
    cur_lexeme: String,
    char_iter: Peekable<Chars<'a>>
}

impl<'a> ScannerState<'a> {
    fn new(source: &'a str) -> ScannerState {
        ScannerState { line: 0, cur_lexeme: String::new(), char_iter: source.chars().peekable() }
    }

    fn has_next(&mut self) -> bool {
        self.char_iter.peek().is_some()
    }

    fn advance(&mut self) -> Option<char> {
        match self.char_iter.next() {
            Some(c) => {
                self.cur_lexeme.push(c);
                Some(c)
            }
            None => None
        }
    }

    fn scan_token(&mut self) -> Option<TokenType> {
        let c = self.advance()?;

        let token_type = match c {
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
            // TODO refactor repetetive calls to self.next
            '!' => if self.next_char_matches('=') {
                self.advance();
                Some(TokenType::BANG_EQUAL)
            } else {
                Some(TokenType::BANG)
            },
            '=' => if self.next_char_matches('=') {
                self.advance();
                Some(TokenType::EQUAL_EQUAL)
            } else {
                Some(TokenType::EQUAL)
            },
            '<' => if self.next_char_matches('=') {
                self.advance();
                Some(TokenType::LESS_EQUAL)
            } else {
                Some(TokenType::LESS)
            },
            '>' => if self.next_char_matches('=') {
                self.advance();
                Some(TokenType::GREATER_EQUAL)
            } else {
                Some(TokenType::GREATER)
            },
            '/' => if self.next_char_matches('/') {
                while self.char_iter.peek().is_some() && !self.next_char_matches('\n') {
                    self.advance();
                }
                None
            } else {
                Some(TokenType::SLASH)
            },
            '\n' => {
                self.line += 1;
                None
            },
            '"' => self.read_string(),
            default => {
                if c.is_digit(10) {
                    self.scan_number()
                } else if c.is_alphabetic() {
                    self.scan_identifier()
                } else {
                    None
                }
            }
        };

        if token_type.is_none() {
            error(self.line, "Unexpected character.");
        }
        token_type
    }


    fn read_string(&mut self) -> Option<TokenType> {
        let mut value = String::new();
        while self.char_iter.peek().is_some() && !self.next_char_matches('"') {
            let x = self.advance()?;
            if (x == '\n') {
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
        while self.char_iter.peek().is_some() && self.char_iter.peek()?.is_digit(10) {
            self.advance();
        }

        // FIXME this is wrong! somehow I have to handle the case 123.sqrt()
        if self.char_iter.peek().is_some() && *self.char_iter.peek()? == '.' {
            self.advance();
        }

        while self.char_iter.peek().is_some() && self.char_iter.peek()?.is_digit(10) {
            self.advance();
        }

        match self.cur_lexeme.as_str().parse::<f64>() {
            Ok(num) => Some(TokenType::NUMBER(num)),
            Err(_) => None
        }
    }

    fn scan_identifier(&mut self) -> Option<TokenType> {
        while self.char_iter.peek().is_some() && self.char_iter.peek()?.is_alphanumeric() {
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
        match self.char_iter.peek() {
            Some(a) => *a == c,
            None => false
        }
    }
}

fn scan_tokens(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut scanner_state = ScannerState::new(source);

    while scanner_state.has_next() {
        // start of a new lexeme
        scanner_state.cur_lexeme.clear();

        let token_type = scanner_state.scan_token();
        match token_type {
            Some(t) => {
                tokens.push(Token {token_type: t, lexeme: scanner_state.cur_lexeme.to_string(), line: scanner_state.line});
            },
            None => ()
        }
    }
    tokens.push(Token {token_type: TokenType::EOF, lexeme: "".to_string(), line: scanner_state.line});
    tokens
}


// TODO: proper fmt::Display trait
// the literal is bundled in the TokenType
#[derive(Debug)]
#[derive(PartialEq)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: i32
}

fn error(line: i32, message: &str) {
    report(line, "", message);
}

fn report(line: i32, location: &str, message: &str) {
    eprintln!("[line {} ] Error {} : {}", line, location, message);
}

#[derive(Debug)]
#[derive(PartialEq)]
enum TokenType {
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
        assert_eq!(result[0], Token { line: 0, lexeme: input.to_string(), token_type:
            expected_token_type }, "{}", input)
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
    // -123.sqrt()
}
