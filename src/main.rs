use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::io;

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
    let scanner = Scanner::new();
    let tokens = scanner.scan_tokens();

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    Ok(())
}

struct Scanner {
    source: String
}

impl Scanner {
    fn scan_tokens(self: &Scanner) -> Vec<Token> {
        Vec::new()
    }

    fn new() -> Scanner {
        Scanner { source: "".to_string() }
    }
}

#[derive(Debug)]
struct Token {
}

fn error(line: i32, message: &str) {
    report(line, "", message);
}

fn report(line: i32, location: &str, message: &str) {
    eprintln!("[line {} ] Error {} : {}", line, location, message);
}
