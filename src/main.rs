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
    char_iter: &'a mut Peekable<Chars<'a>>
}

impl<'a> ScannerState<'a> {
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
        let peek = self.char_iter.peek()?;
        let token_type = if peek.is_digit(10) {
            self.scan_number()
        } else if peek.is_alphabetic() {
            self.advance()?;
            None
        } else {
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
        None
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
    let mut char_iter = source.chars().peekable();
    let mut scanner_state = ScannerState { line: 0, cur_lexeme: String::new(), char_iter:
        &mut char_iter };

    // TODO: ugly solution. rethink this!
    // this part looks like bugs...
    // can not _easily_ use source.lines() iterator, because lox supports multiline strings
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
