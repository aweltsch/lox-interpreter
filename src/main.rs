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
    let mut scanner = Scanner::new();
    let tokens = scanner.scan_tokens(s);

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    Ok(())
}

struct Scanner {
    start: usize,
    current: usize,
    line: i32
}

// TODO do this the rust way! to me this looks too much like the Java source
impl Scanner {
    fn scan_tokens(self: &mut Scanner, source: &str) -> Vec<Token> {
        let mut tokens = Vec::new();

        // TODO: ugly solution. rethink this!
        // this part looks like bugs...
        // can not _easily_ use source.lines() iterator, because lox supports multiline strings
        let mut char_iter = source.chars().peekable();
        while char_iter.peek().is_some() {
            // start of a new lexeme
            self.start = self.current;

            let token_type = self.scan_token(&mut char_iter);
            match token_type {
                Some(t) => {
                    let lexeme = &source[self.start..self.current];
                    tokens.push(Token {token_type: t, lexeme: lexeme.to_string(), line: self.line});
                },
                None => ()
            }
        }
        tokens.push(Token {token_type: TokenType::EOF, lexeme: "".to_string(), line: self.line});
        tokens
    }

    fn new() -> Scanner {
        Scanner { line: 1, start: 0, current: 0 }
    }

    fn scan_token(self: &mut Scanner, char_iter: &mut Peekable<Chars>) -> Option<TokenType> {
        self.current = self.current + 1;
        let c = char_iter.next()?;

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
            '!' => if next_char_matches(char_iter, '=') {
                Some(TokenType::BANG_EQUAL)
            } else {
                Some(TokenType::BANG)
            },
            '=' => if next_char_matches(char_iter, '=') {
                Some(TokenType::EQUAL_EQUAL)
            } else {
                Some(TokenType::EQUAL)
            },
            '<' => if next_char_matches(char_iter, '=') {
                Some(TokenType::LESS_EQUAL)
            } else {
                Some(TokenType::LESS)
            },
            '>' => if next_char_matches(char_iter, '=') {
                Some(TokenType::GREATER_EQUAL)
            } else {
                Some(TokenType::GREATER)
            },
            '/' => if next_char_matches(char_iter, '/') {
                while char_iter.peek().is_some() && !next_char_matches(char_iter, '\n') {
                    char_iter.next();
                }
                None
            } else {
                Some(TokenType::SLASH)
            }
            default => None
        };
        if token_type.is_none() {
            error(self.line, "Unexpected character.");
        }
        token_type
    }
}

fn next_char_matches(char_iter: &mut Peekable<Chars>, c: char) -> bool {
    match char_iter.peek() {
        Some(a) => *a == c,
        None => false
    }
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
